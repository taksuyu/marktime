{-# LANGUAGE DeriveFunctor, GADTs, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, QuasiQuotes, TemplateHaskell, TypeFamilies
             #-}

-- | Stability: Experimental
module Marktime.Common where

import Data.Int
import Data.Text
import Data.Time

import Database.Persist.TH

-- * Utility types

newtype Task
  = Task
    { unTask :: Text }
  deriving (Show)

newtype Project
  = Project
    { unProject :: Text }
  deriving (Show)

showT :: Show a => a -> Text
showT = pack . show

type Time = UTCTime
type Date = Day
type DB = Text

-- * Operation types

data StartOpts
  = StartOpts
    { startTaskId :: Int64 }
  deriving (Show)

data StopOpts
  = StopOpts
    { stopTaskId :: Int64 }
  deriving (Show)

data PauseOpts
  = PauseOpts
    { pauseTaskId :: Int64 }
  deriving (Show)

data AddOpts
  = AddOpts
    { addTaskProject  :: Maybe Project
    , addTaskPriority :: Maybe Int
    , addTaskName     :: Task }
  deriving (Show)

data DeleteOpts
  = DeleteOpts
    { delTaskIds :: [Int64] }
  deriving (Show)

data FinishOpts
  = FinishOpts
    { finishTaskIds :: [Int64] }
  deriving (Show)

data SetOpts
  = SetOpts
    { setTaskShortDesc    :: Maybe Text
    , setTaskLongDesc     :: Maybe Text
    , setTaskDependencies :: Maybe [Int64]
    , setTaskPriority     :: Maybe Int
    , setTaskMilestone    :: Maybe Int64
    , setTaskProject      :: Maybe Int64
    , setTaskDeleted      :: Maybe Bool
    , setTaskFinished     :: Maybe Bool
    , setTaskIds          :: [Int64] }
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

-- * Persistent types

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
    durations [Rational]
    dependencies [TaskStoreId]
    priority Int Maybe
    taskMilestone MilestoneStoreId Maybe
    taskProject ProjectStoreId Maybe
    deleted Bool
    finished Bool
    deriving Show
|]
