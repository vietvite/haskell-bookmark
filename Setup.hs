{-
  David Lettier (C) 2016.
  http://www.lettier.com/
-}

module Main where

import Database
import Controller
import Snap

main :: IO ()
main = do
  -- Create or modify the bookmark database table
  dbMigration
  -- Begin serving all HTTP requests
  quickHttpServe mainRouter
