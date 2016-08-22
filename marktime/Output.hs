{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}

module Output where

-- Pretty printer functions for the ANSI output.

import Prelude                 as P hiding (putStrLn)

import Data.Int
import Data.List               as L
import Data.Maybe
import Data.Monoid
import Data.Text               as T
import Data.Text.IO

import Database.Persist.Sqlite

import Marktime.Common
import Marktime.Database

-- FIXME: For the love of all that is good please use a pretty printer - taksuyu
-- 2016-08-21: Added it to my marktime list. - taksuyu
printTasks :: [Entity TaskStore] -> IO ()
printTasks []
  = putStrLn "No tasks were found."
printTasks ts = do
  putStrLn $ taskHeader <> " - Started"
  mapM_ printTask started
  putStrLn ""
  putStrLn $ taskHeader <> " - Paused"
  mapM_ printTask paused
  putStrLn ""
  putStrLn $ taskHeader <> " - Unfinished"
  mapM_ printTask unfinished
    where
      taskHeader = "Tasks (Key: Description)"
      (started, notStarted) = L.partition (\(Entity _ TaskStore{..}) -> isJust taskStoreStartTime) ts
      (paused, unfinished) = L.partition (\(Entity _ TaskStore{..}) -> not (L.null taskStoreDurations)) notStarted

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
  Left StartTaskNotFound -> t " doesn't exist."

printStopTask :: Int64 -> Either StopTaskError () -> IO ()
printStopTask i = let t = taskShow i in \case
  Right _ -> t " has been stopped."
  Left AlreadyStopped -> t " has already been stopped."
  Left NotStarted -> t " hasn't been started yet."
  Left StopTaskNotFound -> t " doesn't exist."

printPauseTask :: Int64 -> Either PauseTaskError () -> IO ()
printPauseTask i = let t = taskShow i in \case
  Right _ -> t " has been paused."
  Left AlreadyFinished -> t " has already been finished."
  Left PauseNotStarted -> t " hasn't been started yet."
  Left PauseTaskNotFound -> t " doesn't exist."