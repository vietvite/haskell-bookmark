{-
  David Lettier (C) 2016.
  http://www.lettier.com/
-}

{-# LANGUAGE OverloadedStrings #-}

module Controller (
  mainRouter -- We only need to export the `mainRouter` function
             -- This is used in Main.hs
) where

import Database
import Model
import View
import Snap
import Data.Int
import Data.Text
import Data.ByteString
import Data.ByteString.Char8
import Data.ByteString.Lazy
import Data.Maybe
import Data.Aeson
import Control.Monad.IO.Class (liftIO, MonadIO)
import Database.Persist
import Database.Persist.Class

-- Here is a top level router
-- This will define the base and bookmarks routes

mainRouter :: Snap ()
mainRouter =  route [
                  (    "", writeBS "") -- Base / route
                , ("bookmarks", bookmarksRouter) -- /bookmarks route
              ]

bookmarksRouter :: Snap ()
bookmarksRouter =  route [
                  (    "", method GET    bookmarksRouteIndex)  -- Gets a list of bookmarks
                , (    "", method POST   bookmarksRouteCreate) -- Creates a new bookmark
                , ("/:id", method GET    bookmarksRouteShow)   -- Gets a single bookmark by /:id
                , ("/:id", method PUT    bookmarksRouteUpdate) -- Updates a single bookmark by /:id
                , ("/:id", method DELETE bookmarksRouteDelete) -- Deletes a single bookmark by /:id
              ]

bookmarksRouteIndex :: Snap ()
bookmarksRouteIndex = do
  -- Get the limit and start paramters (?limit=:limit&start=:start) if sent
  maybeLimitTo  <- getParam "limit"
  maybeOffsetBy <- getParam "start"
  -- Get a list or array of bookmarks from the database
  bookmarks <- liftIO $ getBookmarks maybeLimitTo maybeOffsetBy
  -- Set the content type to JSON
  -- We will be responding with JSON
  modifyResponse $ setHeader "Content-Type" "application/json"
  -- Write out the JSON response
  writeLBS $ encode $ Prelude.map entityIdToJSON bookmarks

bookmarksRouteShow :: Snap ()
bookmarksRouteShow = do
  -- We will start off assuming the bookmark could not be found
  -- This sets the HTTP status code to 404 (not found)
  set404AndContentType
  -- Get the ID parameter
  maybeBookmarkId <- getParam "id"
  -- Get the bookmark primary key and record
  (bookmarkIdKey, maybeBookmark) <- liftIO $ getBookmarkById maybeBookmarkId
  -- Respond with 200 if the bookmark with ID actually exists
  -- This will write out our JSON response
  resposndWithMaybeBookmark 200 bookmarkIdKey maybeBookmark

bookmarksRouteCreate :: Snap ()
bookmarksRouteCreate = do
  -- Read in the request HTTP body
  body <- readRequestBody 50000
  -- Parse the JSON request body into a `Bookmark` model (record)
  let bookmark = bookmarkJSONToBookmark $ parseBodyToBookmarkJSON body
  -- Insert the bookmark into the database
  bookmarkIdKey <- liftIO $ insertBookmark bookmark
  -- Set the content type to JSON
  modifyResponse $ setHeader "Content-Type" "application/json"
  -- Let the client know that we created a new record (201)
  -- Respond with the newly created bookmark in JSON format
  respondWithBookmark 201 bookmarkIdKey bookmark

bookmarksRouteUpdate :: Snap ()
bookmarksRouteUpdate = do
  set404AndContentType
  maybeBookmarkId <- getParam "id"
  body <- readRequestBody 50000
  -- Parse the request body into `BookmarkJSON`
  let bookmarkJSON = parseBodyToBookmarkJSON body
  -- Update the bookmark if it exists
  (bookmarkIdKey, maybeBookmark) <- liftIO $ updateBookmarkById maybeBookmarkId bookmarkJSON
  -- If the bookmark exists, tell the client OK (200)
  -- Respond with the bookmark JSON or an error message in JSON
  resposndWithMaybeBookmark 200 bookmarkIdKey maybeBookmark

bookmarksRouteDelete :: Snap ()
bookmarksRouteDelete = do
  set404AndContentType
  maybeBookmarkId <- getParam "id"
  -- Delete the bookmark in the database if it exists
  (bookmarkIdKey, maybeBookmark) <- liftIO $ deleteBookmarkById maybeBookmarkId
  -- If the bookmark exists, resond with 200 and the bookmark in JSON form
  -- Otherwise respond with 404 (not found) and an error message in JSON format
  resposndWithMaybeBookmark 200 bookmarkIdKey maybeBookmark

set404AndContentType :: Snap ()
set404AndContentType = do
  -- Set the HTTP status code to 404 (not found)
  modifyResponse $ setResponseCode 404
  -- Set the content type as JSON
  -- This will let the client know what kind of data is being returned
  -- in the HTTP response body
  modifyResponse $ setHeader "Content-Type" "application/json"

parseBodyToBookmarkJSON :: Data.ByteString.Lazy.ByteString -> BookmarkJSON
-- Parse a raw HTTP body into a `BookmarkJSON` record
parseBodyToBookmarkJSON body = fromMaybe (BookmarkJSON (Just "") (Just "")) (decode body :: Maybe BookmarkJSON)

resposndWithMaybeBookmark :: Int -> Key Bookmark -> Maybe Bookmark -> Snap()
resposndWithMaybeBookmark code bookmarkIdKey maybeBookmark = case maybeBookmark of
    -- Bookmark not found?
    Nothing -> writeBS ("{\"error\": \"Not found.\"}" :: Data.ByteString.ByteString)
    -- Bookmark found?
    -- The code is the HTTP status code
    Just bookmark -> respondWithBookmark code bookmarkIdKey bookmark

respondWithBookmark :: Int -> Key Bookmark -> Bookmark -> Snap()
respondWithBookmark code bookmarkIdKey bookmark = do
  -- Set the HTTP status code
  modifyResponse $ setResponseCode code
  -- Write out the bookmark in JSON format into the response body
  writeLBS $ bookmarkAsJSONLBS bookmarkIdKey bookmark
