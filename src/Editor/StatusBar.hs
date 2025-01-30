{-# LANGUAGE GADTs #-}

module Editor.StatusBar where

import Prelude

data StatusMode where
  Exception :: StatusMode
  NoException :: StatusMode
  deriving (Show, Eq)

data StatusBar = StatusBar
  { statusMode :: StatusMode,
    errorMessage :: String
  }
  deriving (Show)
