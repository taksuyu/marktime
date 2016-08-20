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
taskShow :: Int64 -> Text -> IO ()
taskShow i t = putStrLn $ "Task " <> showT i <> t

printStartTask :: Int64 -> Either StartTaskError () -> IO ()
printStartTask i = let t = taskShow i in \case
  Right _ -> t " has been started."
  Left AlreadyStarted -> t " has already been started."
  Left StartTaskNotFound -> t " doesn't exist"

printStopTask :: Int64 -> Either StopTaskError () -> IO ()
printStopTask i = let t = taskShow i in \case
  Right _ -> t " has been stopped."
  Left AlreadyStopped -> t " has already been stopped."
  Left NotStarted -> t " hasn't been started yet."
  Left StopTaskNotFound -> t " doesn't exist."