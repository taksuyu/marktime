{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-- | Stability: Experimental
module Marktime.Database where

-- | Database transactions.

import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger

import Data.Text               as T
import Data.Time

import Database.Persist
import Database.Persist.Sqlite

import System.Directory
import System.FilePath

import Marktime.Common

defaultTaskStore :: Text -> Time -> TaskStore
defaultTaskStore text time
  = TaskStore text Nothing time Nothing Nothing mempty Nothing Nothing Nothing False False

-- | If we are given a database path, then we'll trust the user that it is
-- correct. Otherwise we have to make sure the default directories are in proper
-- order.
checkDBLocation :: Default FilePath -> IO DB
checkDBLocation (Non path) = pure . pack $ path
checkDBLocation (Default)  = checkDefaultDBlocation

-- | our default database and we'll make sure the directory already exists since
-- `runSqlite` doesn't like it when it doesn't.
checkDefaultDBlocation :: IO DB
checkDefaultDBlocation = do
  -- FIXME: Until directory gets updated to 1.2.3.0 or greater we can't use
  -- getXdgDirectory XdgConfig "marktime"
  configdir <- fmap (</> ".cache/marktime") getHomeDirectory

  -- ~/.cache should exist so no need to create parents.
  createDirectoryIfMissing False configdir

  -- returning the location of our database since it's the same as building the
  -- filepath and extracting it after our check.
  pure $ pack (configdir </> "marktime.sqlite3")

migrateDB :: DB -> IO DB
migrateDB db = do
  runDB db $ runMigration migrateAll
  pure db

runDB :: DB -> SqlPersistT IO b -> IO b
runDB dbPath dbAction = do
  runNoLoggingT . withSqlitePool dbPath 1 $
    \pool -> liftIO $ runSqlPool dbAction pool

runDBGetTime :: DB -> (UTCTime -> SqlPersistT IO b) -> IO b
runDBGetTime db dbAction = do
  currentTime <- getCurrentTime
  runDB db $ dbAction currentTime

data StartTaskError
  = AlreadyStarted
  | StartTaskNotFound
  deriving (Eq, Show)

startTask :: DB -> Key TaskStore -> IO (Either StartTaskError ())
startTask db key = runDBGetTime db $ \ time -> do
  task <- get key
  case task of
    Just TaskStore{..} ->
      case taskStoreStartTime of
        Nothing -> do
          update key [TaskStoreStartTime =. Just time]
          pure (Right ())
        Just _ ->
          pure (Left AlreadyStarted)
    Nothing ->
      pure (Left StartTaskNotFound)

data StopTaskError
  = AlreadyStopped
  | NotStarted
  | StopTaskNotFound
  deriving (Eq, Show)

stopTask :: DB -> Key TaskStore -> IO (Either StopTaskError ())
stopTask db key = runDBGetTime db $ \time -> do
  task <- get key
  case task of
    Just TaskStore{..} ->
      case (taskStoreStartTime, taskStoreStopTime) of
        (Just _, Nothing) -> do
          update key [TaskStoreStopTime =. Just time, TaskStoreFinished =. True]
          pure (Right ())
        (Just _, Just _) ->
          pure (Left AlreadyStopped)
        (Nothing, _) ->
          pure (Left NotStarted)
    Nothing ->
      pure (Left StopTaskNotFound)

-- FIXME: UGLY! Change that generation please.
insertTask :: DB -> AddOpts -> IO (Key TaskStore)
insertTask db AddOpts{..} = runDBGetTime db $ \time ->
  insert $ (defaultTaskStore (unTask addTaskName) time){ taskStorePriority = addTaskPriority }

deleteTask :: DB -> Key TaskStore -> IO ()
deleteTask db key = runDB db $ update key [TaskStoreDeleted =. True]

finishTask :: DB -> Key TaskStore -> IO ()
finishTask db key = runDB db $ update key [TaskStoreFinished =. True]

getAllTasks :: DB -> IO [Entity TaskStore]
getAllTasks db = runDB db $ selectList [] []

getUncompletedTasks :: DB -> IO [Entity TaskStore]
getUncompletedTasks db = runDB db $ selectList
  [TaskStoreDeleted ==. False, TaskStoreFinished ==. False] []

taskByKey :: DB -> Key TaskStore -> IO (Maybe TaskStore)
taskByKey db = runDB db . get
