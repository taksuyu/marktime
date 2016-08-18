{-# LANGUAGE ExistentialQuantification, GADTs, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, OverloadedStrings, QuasiQuotes,
             RecordWildCards, TemplateHaskell, TypeFamilies #-}

-- | Stability: Experimental
module Marktime.Database where

-- | Database transactions.

import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger

import Data.Text               as T
import Data.Time

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import System.Directory
import System.FilePath

import Marktime.Common

showT :: Show a => a -> Text
showT = pack . show

type Time = UTCTime
type Date = Day
type DB = Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ProjectStore
    projectDesc Text
MilestoneStore
    milestoneDesc Text
    milestoneProject ProjectStoreId
    deriving Show
TaskStore
    taskDesc Text
    dueDate Date Maybe
    creationTime Time
    startTime Time Maybe
    stopTime Time Maybe
    dependencies [TaskStoreId]
    priority Int Maybe
    taskMilestone MilestoneStoreId Maybe
    taskProject ProjectStoreId Maybe
    deriving Show
|]

defaultTaskStore :: Text -> Time -> TaskStore
defaultTaskStore text time
  = TaskStore text Nothing time Nothing Nothing mempty Nothing Nothing Nothing

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

  -- ~/.config should exist so no need to create parents.
  createDirectoryIfMissing False configdir

  -- returning the location of our database since it's the same as building the
  -- filepath and extracting it after our check.
  pure $ pack (configdir </> "marktime.sqlite3")

migrateDB :: DB -> IO DB
migrateDB db = pure db <$ runDB db $ runMigration migrateAll

-- FIXME: Perhaps we could do our migrations on startup and not with every
-- database transaction?
runDB :: Text -> SqlPersistT IO b -> IO b
runDB dbPath dbAction = do
  runNoLoggingT . withSqlitePool dbPath 1 $
    \pool -> liftIO $ runSqlPool dbAction pool

data TaskError
  = AlreadyThere
  | TaskNotFound
  deriving (Eq, Show)

-- | All time fields have the same property that you want to check if it's
-- already there before ever setting the current time.
updateTimeOnTask
  :: (TaskStore -> Maybe Time) -> EntityField TaskStore (Maybe Time)
  -> DB -> Key TaskStore -> IO (Either TaskError ())
updateTimeOnTask timeField entityField db key = do
  currentTime <- getCurrentTime
  runDB db $ do
    task <- get key
    case task of
      Just a ->
        case timeField a of
          Nothing -> do
            update key [entityField =. Just currentTime]
            pure $ Right ()
          Just _ ->
            pure (Left AlreadyThere)
      Nothing ->
        pure (Left TaskNotFound)

startTask :: DB -> Key TaskStore -> IO (Either TaskError ())
startTask = updateTimeOnTask taskStoreStartTime TaskStoreStartTime

stopTask :: DB -> Key TaskStore -> IO (Either TaskError ())
stopTask = updateTimeOnTask taskStoreStopTime TaskStoreStopTime

insertTask :: DB -> Task -> IO (Key TaskStore)
insertTask db (Task t) = do
  currentTime <- getCurrentTime
  runDB db .
    insert $ defaultTaskStore t currentTime

getAllTasks :: DB -> IO [Entity TaskStore]
getAllTasks db = runDB db $ selectList [] []

taskByKey :: DB -> Key TaskStore -> IO (Maybe TaskStore)
taskByKey db = runDB db . get
