{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Output where

-- Pretty printer functions for the ANSI output.

import Prelude                 as P hiding (putStrLn)

import Data.Monoid
import Data.Text               as T
import Data.Text.IO

import Database.Persist.Sqlite

import Marktime.Database

listTaskHeader :: IO ()
listTaskHeader = putStrLn "Tasks (Key: Description)"

printTask :: Entity TaskStore -> IO ()
printTask (Entity TaskStoreKey{..} TaskStore{..})
  = putStrLn $ let keylength = T.length keyText
                   keyText = showT (unSqlBackendKey unTaskStoreKey)
               in leftpad (10 - keylength) keyText  <> ": " <> taskStoreTaskDesc

-- FIXME: Text builder and fold
leftpad :: Int -> Text -> Text
leftpad 0 str' = str'
leftpad n str' | n > 0 = " " <> leftpad (n - 1) str'
               | otherwise = leftpad 0 str'
