{-# LANGUAGE OverloadedStrings #-}

module Parser where

-- | Command-line parser.

import Prelude                  as P

import Data.Int
import Data.Text                as T

import Options.Applicative
import Options.Applicative.Text

import Marktime.Common

int64Argument :: Mod ArgumentFields Int64 -> Parser Int64
int64Argument = argument auto

filepathOption :: Mod OptionFields FilePath -> Parser FilePath
filepathOption = option auto

data MarktimeOpts
  = MarktimeOpts
    { mtQuiet   :: Default Bool
      -- ^ Suppress output?
    , mtDatabase :: Default FilePath
      -- ^ Path to database

      -- Commands
    , mtCommand :: MarktimeCommand }
  deriving (Show)

data MarktimeCommand
  = StartCommand StartOpts
  | StopCommand StopOpts
  | AddCommand AddOpts
  | DeleteCommand DeleteOpts
  | ListCommand
  | InfoCommand InfoOpts
  | ReportCommand ReportOpts
  deriving (Show)

startOpts :: Parser MarktimeCommand
startOpts = fmap StartCommand $ pure StartOpts
  <*> int64Argument
  (metavar "KEY"
    <> help "Start a task by it's key.")

stopOpts :: Parser MarktimeCommand
stopOpts = fmap StopCommand $ pure StopOpts
  <*> int64Argument
  (metavar "KEY"
    <> help "Stop a task by it's key.")

addOpts :: Parser MarktimeCommand
addOpts = fmap AddCommand $ pure AddOpts
  <*> (optional . fmap Project . textOption)
  (long "project"
    <> short 'p'
    <> metavar "PROJECT"
    <> help "Assign the new task to a project.")

  <*> (fmap Task . textArgument)
  (metavar "NAME"
    <> help "Create a task with the given name.")

delOpts :: Parser MarktimeCommand
delOpts = fmap DeleteCommand $ pure DeleteOpts
  <*> int64Argument
  (metavar "KEY"
    <> help "Delete a task by it's key.")

listOpts :: Parser MarktimeCommand
listOpts = pure ListCommand

infoOpts :: Parser MarktimeCommand
infoOpts = fmap InfoCommand $ pure InfoOpts
  <*> int64Argument
  (metavar "KEY"
    <> help "Info on the task by it's key.")

reportOpts :: Parser MarktimeCommand
reportOpts = fmap ReportCommand $ pure ReportOpts
  <*> (optional . fmap ReportOutput . textOption)
  (long "output"
    <> short 'o'
    <> metavar "FILE"
    <> help "Write the output to a file.")

  <*> (fmap ReportLayout . textArgument)
  (metavar "LAYOUT"
    <> (help . P.concat)
    [ "Executable to process information into a presentable form."
    , "These can be found in ~/.cache/marktime/layouts"])

data StartOpts
  = StartOpts
    { startTaskId :: Int64 }
  deriving (Show)

data StopOpts
  = StopOpts
    { stopTaskId :: Int64 }
  deriving (Show)

data AddOpts
  = AddOpts
    { addTaskProject :: Maybe Project
    , addTaskName    :: Task }
  deriving (Show)

data DeleteOpts
  = DeleteOpts
    { delTaskId :: Int64 }
  deriving (Show)

data InfoOpts
  = InfoOpts
    { infoTaskId :: Int64 }
  deriving (Show)

data ReportOpts
  = ReportOpts
    { reportOutput :: Maybe ReportOutput
    , reportLayout :: ReportLayout }
  deriving (Show)

newtype ReportOutput
  = ReportOutput Text
  deriving (Show)

newtype ReportLayout
  = ReportLayout Text
  deriving (Show)

marktimeParser :: Parser MarktimeOpts
marktimeParser = pure MarktimeOpts
  <*> (flag Default (Non True))
  (long "quiet"
    <> short 'q'
    <> help "Suppress output.")

  <*> (optionalDefault . filepathOption)
  (long "database"
    <> short 'd'
    <> metavar "FILE"
    <> help "Location of the database that should be used.")

  <*> subparser
  (command "start"
    (info startOpts
     (progDesc "Start tracking a task."))

    <> command "stop"
    (info stopOpts
     (progDesc "Stop tracking a task."))

    <> command "add"
    (info addOpts
     (progDesc "Add a new task to work on."))

    <> command "delete"
    (info delOpts
     (progDesc "Delete a task."))

    <> command "list"
    (info listOpts
     (progDesc "List all current tasks."))

    <> command "info"
    (info infoOpts
     (progDesc "Info on a task."))

    <> command "report"
    (info reportOpts
     (progDesc "Output a report.")))

runMTParser :: IO MarktimeOpts
runMTParser = customExecParser (prefs showHelpOnError)
  (info (helper <*> marktimeParser)
    (fullDesc
      <> progDesc "Time and task tracking software."))
