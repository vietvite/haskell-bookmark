{-
  David Lettier (C) 2016.
  http://www.lettier.com/
-}

{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric #-}

-- Export our view (`BookmarkJSON`) and two helper functions for
-- turning some JSON into a `Bookmark` record or
-- turning a `Bookmark` record into a JSON string

module View (
    BookmarkJSON(..)
  , bookmarkJSONToBookmark
  , bookmarkAsJSONLBS -- LBS stands for Lazy Byte String
) where

-- Our custom Model module

import Model

-- Build dependencies

import GHC.Generics
import Data.Int
import Data.Text
import Data.ByteString
import Data.ByteString.Char8
import Data.ByteString.Lazy
import Data.Maybe
import Data.Aeson
import Data.Default.Class
import Database.Persist
import Database.Persist.Class

-- Our "view" or `BookmarkJSON` record

data BookmarkJSON = BookmarkJSON {
    bookmarkJSONTitle :: Maybe String
  , bookmarkJSONUrl :: Maybe String
} deriving (Show, Generic)

-- Here we defined how to parse a JSON string "{\"title\": \"...\", \"url\": \"...\"}"
-- into a `BookmarkJSON` record

instance FromJSON BookmarkJSON where
  parseJSON (Object v) =
    BookmarkJSON <$> v .:?  "title" -- .:? is syntax for parsing a JSON string field into Maybe String
                 <*> v .:?  "url"   -- The JSON string may not have "{\"url\": \"...\"}"
                                    -- If that is the case, `bookmarkJSONURL` will be `Nothing`

-- Here we define how to take a `BookmarkJSON` record
-- and turn it into JSON {"title": "...", "url": "..."}
-- For example:
-- > let x = BookmarkJSON {bookmarkJSONTitle = Just "one", bookmarkJSONUrl = Just "two"}
-- > toJSON x
-- Object (fromList [("url",String "two"),("title",String "one")])
-- > encode $ toJSON x
-- "{\"url\":\"two\",\"title\":\"one\"}"

instance ToJSON BookmarkJSON where
  toJSON (BookmarkJSON title url) = object ["title" .= title, "url" .= url]

bookmarkJSONToBookmark :: BookmarkJSON -> Bookmark
bookmarkJSONToBookmark bookmarkJSON = Bookmark titleJSONToTitle urlJSONToUrl
  where
    -- If the JSON didn't have a title, just set the title to an empty string
    titleJSONToTitle = fromMaybe "" $ bookmarkJSONTitle bookmarkJSON
    -- If the JSON didn't have a URL, just set the title to an empty string
    urlJSONToUrl = fromMaybe "" $ bookmarkJSONUrl bookmarkJSON

bookmarkAsJSONLBS :: Key Bookmark -> Bookmark -> Data.ByteString.Lazy.ByteString
-- Convert a bookmark primary key and `Bookmark` record to a JSON lazy byte string
-- "{\"id\": 1, \"title\": \"...\", \"url\": \"...\"}"
bookmarkAsJSONLBS k b = encode . entityIdToJSON $ Entity k b
