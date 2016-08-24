{-# LANGUAGE OverloadedStrings #-}

module Parser where

-- | Command-line parser.

import Prelude                  as P

import Data.Int

import Options.Applicative
import Options.Applicative.Text

import Marktime.Common

int64Argument :: Mod ArgumentFields Int64 -> Parser Int64
int64Argument = argument auto

intOption :: Mod OptionFields Int -> Parser Int
intOption = option auto

filepathOption :: Mod OptionFields FilePath -> Parser FilePath
filepathOption = option auto

data MarktimeOpts
  = MarktimeOpts
    { mtQuiet   :: Bool
      -- ^ Suppress output?
    , mtDatabase :: Maybe FilePath
      -- ^ Path to database

      -- Commands
    , mtCommand :: MarktimeCommand }
  deriving (Show)

data MarktimeCommand
  = StartCommand StartOpts
  | StopCommand StopOpts
  | PauseCommand PauseOpts
  | AddCommand AddOpts
  | DeleteCommand DeleteOpts
  | FinishCommand FinishOpts
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

pauseOpts :: Parser MarktimeCommand
pauseOpts = fmap PauseCommand $ pure PauseOpts
  <*> int64Argument
  (metavar "KEY"
    <> help "Pause a task by it's key.")

addOpts :: Parser MarktimeCommand
addOpts = fmap AddCommand $ pure AddOpts
  <*> (optional . fmap Project . textOption)
  (long "project"
    <> short 'p'
    <> metavar "PROJECT"
    <> help "Assign the new task to a project.")

  <*> (optional . intOption)
  (long "priority"
    <> short 'P'
    <> metavar "NUM"
    <> help "Assign the new task a priority; lowest is displayed first.")

  <*> (fmap Task . textArgument)
  (metavar "NAME"
    <> help "Create a task with the given name.")

delOpts :: Parser MarktimeCommand
delOpts = fmap DeleteCommand $ pure DeleteOpts
  <*> some (int64Argument
  (metavar "KEY"
    <> help "Delete tasks by it's key."))

finishOpts :: Parser MarktimeCommand
finishOpts = fmap FinishCommand $ pure FinishOpts
  <*> some (int64Argument
  (metavar "KEY"
    <> help "Finish tasks by their key."))

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

marktimeParser :: Parser MarktimeOpts
marktimeParser = pure MarktimeOpts
  <*> (flag False True)
  (long "quiet"
    <> short 'q'
    <> help "Suppress output.")

  <*> (optional . filepathOption)
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

    <> command "pause"
    (info pauseOpts
     (progDesc "Pause tracking a task."))

    <> command "add"
    (info addOpts
     (progDesc "Add a new task to work on."))

    <> command "delete"
    (info delOpts
     (progDesc "Delete a task."))

    <> command "finish"
    (info finishOpts
     (progDesc "Finish a task."))

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
