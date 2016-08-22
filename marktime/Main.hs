{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}

module Main where

import Prelude                 as P hiding (putStrLn)

import Data.Monoid
import Data.Text.IO

import Database.Persist.Sqlite

import Marktime.Common
import Marktime.Database

import Output
import Parser

-- FIXME: REMOVE THIS
todoMessage :: IO ()
todoMessage = putStrLn "This has yet to be implemented."

main :: IO ()
main = runMTParser >>=
  \ MarktimeOpts{..} -> do
    db <- migrateDB =<< checkDBLocation mtDatabase
    case mtCommand of
      StartCommand StartOpts{..} ->
        startTask db (toSqlKey startTaskId) >>=
        printStartTask startTaskId
      StopCommand StopOpts{..} ->
        stopTask db (toSqlKey stopTaskId) >>=
        printStopTask stopTaskId
      PauseCommand PauseOpts{..} ->
        pauseTask db (toSqlKey pauseTaskId) >>=
        printPauseTask pauseTaskId
      AddCommand a -> do
        taskKey <- insertTask db a
        putStrLn $ showT (fromSqlKey taskKey) <> " has been added."
      DeleteCommand DeleteOpts{..} ->
        mapM_ (deleteTask db . toSqlKey) delTaskIds
      FinishCommand FinishOpts{..} ->
        mapM_ (finishTask db . toSqlKey) finishTaskIds
      ListCommand -> do
        printTasks =<< getUncompletedTasks db
      InfoCommand InfoOpts{..} ->
        taskByKey db (toSqlKey infoTaskId) >>=
        \case
          Just a -> print a
          Nothing -> putStrLn $ "There was no task associated with: " <> showT infoTaskId
      ReportCommand ReportOpts{..} ->
        todoMessage
