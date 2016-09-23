{-# LANGUAGE DeriveFunctor, GADTs, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, QuasiQuotes, TemplateHaskell, TypeFamilies
             #-}

-- | Stability: Experimental
module Marktime.Common where

import Data.Int
import Data.Text
import Data.Time

import Database.Persist
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
    , setTaskFnished      :: Maybe Bool
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

data TimeTo
  = TimeTo (Time, Time)
    -- ^ Time from one timestamp to another.
  | TimeDiff Rational
    -- ^ This is included if we want to talk about time as a difference. Also in
    -- the case that we don't have the timestamps of each time.
  deriving (Show)

instance PersistField TimeTo where
  toPersistValue (TimeTo   t) = toPersistValue t
  toPersistValue (TimeDiff d) = toPersistValue d

  fromPersistValue a@(PersistList _) =
    case fromPersistValue a of
      Right (PersistUTCTime t, PersistUTCTime u) -> Right $ TimeTo (t, u)
      e -> Left . pack $ "Expected Right (PersistList [UTCTime, UTCTime]), recieved: " ++ show e
  fromPersistValue (PersistRational r) =
    Right $ TimeDiff r
  fromPersistValue e =
    Left . pack $ "Expected PersistList [UTCTime, UTCTime] or PersistRational, recieved: " ++ show e

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ProjectStore
    projectDesc Text
    deriving Show
MilestoneStore
    milestoneDesc Text
    deriving Show
TaskStore
    taskDesc Text
    dueDate Date Maybe
    creationTime Time
    startTime Time Maybe
    stopTime Time Maybe
    durations [TimeTo]
    dependencies [TaskStoreId]
    priority Int Maybe
    taskMilestone MilestoneStoreId Maybe
    taskProject ProjectStoreId Maybe
    deleted Bool
    finished Bool
    deriving Show
|]
