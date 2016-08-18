{-# LANGUAGE ExistentialQuantification, GADTs, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, OverloadedStrings, QuasiQuotes,
             RecordWildCards, TemplateHaskell, TypeFamilies #-}

-- | Stability: Experimental
module Marktime.Database where

-- | Database transactions.

import Prelude                 as P hiding (putStrLn)

import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger

import Data.Monoid
import Data.Text               as T
import Data.Text.IO
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

newtype Location a
  = Location Text
  deriving (Show)

data DB

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
checkDBLocation :: Default FilePath -> IO (Location DB)
checkDBLocation (Non path) = pure . Location . pack $ path
checkDBLocation (Default)  = checkDefaultDBlocation

-- | our default database and we'll make sure the directory already exists since
-- `runSqlite` doesn't like it when it doesn't.
checkDefaultDBlocation :: IO (Location DB)
checkDefaultDBlocation = do
  -- FIXME: Until directory gets updated to 1.2.3.0 or greater we can't use
  -- getXdgDirectory XdgConfig "marktime"
  configdir <- fmap (</> ".cache/marktime") getHomeDirectory

  -- ~/.config should exist so no need to create parents.
  createDirectoryIfMissing False configdir

  -- returning the location of our database since it's the same as building the
  -- filepath and extracting it after our check.
  pure . Location $ pack (configdir </> "marktime.sqlite3")

-- FIXME: Perhaps we could do our migrations on startup and not with every
-- database transaction?
runDB :: Text -> SqlPersistT IO b -> IO b
runDB dbPath dbAction = do
  runNoLoggingT . withSqlitePool dbPath 1 $
    \pool -> liftIO $ runSqlPool migrateAction pool
    where
      migrateAction = do
        runMigration migrateAll
        dbAction

insertTask :: Task -> IO (Key TaskStore)
insertTask (Task t) = do
  currentTime <- getCurrentTime
  (Location db) <- checkDefaultDBlocation
  runDB db .
    insert $ defaultTaskStore t currentTime

-- TODO: Binded to a display action
listTasks :: IO ()
listTasks = do
  (Location db) <- checkDefaultDBlocation
  tasks <- runDB db $ selectList [] []
  putStrLn "Tasks (Key: Description)"
  mapM_ printTask tasks
      where
        printTask (Entity TaskStoreKey{..} TaskStore{..})
          = putStrLn $ let keylength = T.length keyText
                           keyText = showT (unSqlBackendKey unTaskStoreKey)
                       in leftpad (10 - keylength) keyText  <> ": " <> taskStoreTaskDesc

        -- FIXME: Text builder and fold
        leftpad 0 str' = str'
        leftpad n str' | n > 0 = " " <> leftpad (n - 1) str'
                       | otherwise = leftpad 0 str'

taskByKey :: Location DB -> Key TaskStore -> IO (Maybe TaskStore)
taskByKey (Location db) = runDB db . get
