{-# LANGUAGE DeriveFunctor, GADTs, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, QuasiQuotes, TemplateHaskell, TypeFamilies
             #-}

-- | Stability: Experimental
module Marktime.Common where

import Control.Applicative

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

data Default a
  = Default
  | Non a
  deriving ( Eq, Show
           , Functor )

instance Applicative Default where
  pure = Non

  Default <*> _ = Default
  Non fn  <*> a = fmap fn a

optionalDefault :: Alternative f => f a -> f (Default a)
optionalDefault v = Non <$> v <|> pure Default

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

data AddOpts
  = AddOpts
    { addTaskProject  :: Maybe Project
    , addTaskPriority :: Maybe Int
    , addTaskName     :: Task }
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
    dependencies [TaskStoreId]
    priority Int Maybe
    taskMilestone MilestoneStoreId Maybe
    taskProject ProjectStoreId Maybe
    deriving Show
|]
