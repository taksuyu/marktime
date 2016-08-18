{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module Output where

-- Pretty printer functions for the ANSI output.

import Prelude                 as P hiding (putStrLn)

import Data.Int
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

startStopTask :: Int64 -> Text -> Either TaskError () -> IO ()
startStopTask i t = let taskShow t' = putStrLn $ "Task " <> showT i <> t' in
  \case
    Right _ -> taskShow (" has been " <> t <> ".")
    Left AlreadyThere -> taskShow (" has already been " <> t <> ".")
    Left TaskNotFound -> taskShow (" doesn't exist")