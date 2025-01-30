module Editor.StatusBar where

import Prelude

data StatusMode = Exception | NoException deriving (Show, Eq)
 
data StatusBar = StatusBar {
  statusMode :: StatusMode,
  errorMessage :: String
} deriving Show

-- comentario
