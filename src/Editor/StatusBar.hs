module Editor.StatusBar where

import Prelude

data SBarMode = Exception | NoException deriving (Show, Eq)
 
data StatusBar = StatusBar {
  statusMode :: SBarMode,
  errorMessage :: String
} deriving Show

