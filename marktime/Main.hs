{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}

module Main where

import Prelude                 as P hiding (putStrLn)

import Data.Monoid
import Data.Text.IO

import Database.Persist.Sqlite

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
        startStopTask startTaskId "started"
      StopCommand StopOpts {..} ->
        stopTask db (toSqlKey stopTaskId) >>=
        startStopTask stopTaskId "stopped"
      AddCommand AddOpts{..} ->
        () <$ insertTask db addTaskName
      ListCommand -> do
        tasks <- getAllTasks db
        listTaskHeader
        mapM_ printTask tasks
      InfoCommand InfoOpts{..} ->
        taskByKey db (toSqlKey infoTaskId) >>=
        \case
          Just a -> print a
          Nothing -> putStrLn $ "There was no task associated with: " <> showT infoTaskId
      ReportCommand ReportOpts{..} ->
        todoMessage
