{-# LANGUAGE DeriveFunctor #-}

-- | Stability: Experimental
module Marktime.Common where

import Control.Applicative

import Data.Text

newtype Task
  = Task Text
  deriving (Show)

newtype Project
  = Project Text
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