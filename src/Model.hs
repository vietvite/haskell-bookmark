{-
  David Lettier (C) 2016.
  http://www.lettier.com/
-}

-- Various language extensions needed to compile Model.hs
-- Feel free to lookup each one

{-# LANGUAGE
    OverloadedStrings
  , EmptyDataDecls
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , DeriveGeneric
  , GADTs
  , TypeFamilies
  , TemplateHaskell
  , QuasiQuotes
  , FlexibleInstances
  , FlexibleContexts
  , StandaloneDeriving #-}

-- Export our `Bookmark` record (model),
-- the entity (bookmark) definition,
-- and the entity fields' setters and getters

module Model (
    Bookmark(..)
  , entityDefs
  , EntityField(..)
) where

-- Needed for encoding and decoding to/from JSON

import GHC.Generics
import Data.Aeson
import Data.Default.Class

-- Needed for generating our bookmark entity

import Database.Persist
import Database.Persist.Class
import Database.Persist.TH

-- Generates our `BookmarkEntity` instance and `Bookmark` record

share [mkPersist sqlSettings, mkSave "entityDefs"][persistLowerCase|
  Bookmark
    -- Two fields
    title String
    url   String
    deriving Show Generic
|]

-- Defines the ToJSON interface for our `Bookmark` record
-- This will take a `Bookmark` record and convert it to JSON
-- For example:
-- > let x = Bookmark {bookmarkTitle = "one", bookmarkUrl = "two"}
-- > toJSON x
-- Object (fromList [("url",String "two"),("title",String "one")])
-- > encode $ toJSON x
-- "{\"url\":\"two\",\"title\":\"one\"}"

instance ToJSON Bookmark where
  toJSON (Bookmark title url) = object ["title" .= title, "url" .= url]
