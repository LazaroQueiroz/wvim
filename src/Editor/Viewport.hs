module Editor.Viewport where

import Utils

data Viewport = Viewport
  { rows :: Int,
    columns :: Int,
    initialRow :: Int,
    initialColumn :: Int
  }
  deriving (Show, Eq)


updateViewport :: Viewport -> (Int, Int) -> [Int] -> Bool -> Viewport
updateViewport viewport (x', y') lineSizes isInsert
  | x' < initialRow' = scrollUp viewport (initialRow' - x')
  | (x' - initialRow') > (rows' - 2) = scrollDown viewport (x' - initialRow' - rows' + 2)
  | y' < initialColumn' = scrollLeft viewport (initialColumn' - y')
  | (y' - initialColumn') > (columns' - 2) = scrollRight viewport (y' - initialColumn' - columns' + 2) 
  -- -- | y' - (initialColumn viewport) >= (columns' - 1) && (direction == 'l' || isInsert) && (columns' - 1) + 1 + initialColumn' < nth (x' + 1) lineSizes = scrollRight viewport 1
  -- | y' - (initialColumn viewport) >= (columns' - 1) && (direction == 'l' || isInsert) = scrollRight viewport 1
  -- | x' - (initialRow viewport) == (rows' - 2) && direction `elem` ['\n'] = scrollDown viewport 1
  -- | x' - (initialRow viewport) == (rows' - 2) && direction `elem` ['j'] && (rows' - 2) + 1 + initialRow' < length lineSizes = scrollDown viewport 1
  -- | x' - (initialRow viewport) == 0 && initialRow' > 0 && direction == 'k' = scrollUp viewport 1
  -- -- | y' == 0 && x' - (initialRow viewport) == 0 && direction == '\DEL' && initialRow' > 0 = scrollUp viewport 1
  -- | y' - (initialColumn viewport) == 0 && direction == '\DEL' && initialRow' > 0 = scrollUp viewport 1
  -- | y' - (initialColumn viewport) == 0 && direction `elem` ['h', '\DEL'] && initialColumn' > 0 = scrollLeft viewport 1
  | otherwise = viewport
  where
    (Viewport rows' columns' initialRow' initialColumn') = viewport

defaultViewport :: Int -> Int -> Viewport
defaultViewport rows' columns' = Viewport rows' columns' 0 0

scrollUp :: Viewport -> Int -> Viewport
scrollUp viewport multiplier = viewport {initialRow = initialRow viewport - multiplier}

scrollDown :: Viewport -> Int -> Viewport
scrollDown viewport multiplier = viewport {initialRow = initialRow viewport + multiplier}

scrollLeft :: Viewport -> Int -> Viewport
scrollLeft viewport multiplier = viewport {initialColumn = initialColumn viewport - multiplier}

scrollRight :: Viewport -> Int -> Viewport
scrollRight viewport multiplier = viewport {initialColumn = initialColumn viewport + multiplier}

resizeViewport :: Viewport -> Int -> Int -> Viewport
resizeViewport viewport newRows newColumns = viewport {rows = newRows, columns = newColumns}
