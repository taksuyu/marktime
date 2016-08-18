{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}

module Main where

import Prelude                 as P hiding (putStrLn)

import Data.Monoid
import Data.Text.IO

import Database.Persist.Sqlite

import Marktime.Database

import Parser

-- FIXME: REMOVE THIS
todoMessage :: IO ()
todoMessage = putStrLn "This has yet to be implemented."

main :: IO ()
main = runMTParser >>=
  \ MarktimeOpts{..} -> do
    db <- checkDBLocation mtDatabase
    case mtCommand of
      StartCommand StartOpts{..}
        -> todoMessage
      StopCommand StopOpts {..}
        -> todoMessage
      AddCommand AddOpts{..}
        -> () <$ insertTask addTaskName
      ListCommand
        -> listTasks
      InfoCommand InfoOpts{..}
        -> taskByKey db (toSqlKey infoTaskId) >>=
           \case
             Just a -> print a
             Nothing -> putStrLn $ "There was no task associated with: " <> showT infoTaskId
      ReportCommand ReportOpts{..}
        -> todoMessage
