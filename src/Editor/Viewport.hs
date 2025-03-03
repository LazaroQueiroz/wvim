module Editor.Viewport where

data Viewport = Viewport
  { rows :: Int,
    columns :: Int,
    initialRow :: Int,
    initialColumn :: Int
  }
  deriving (Show, Eq)

updateViewport :: Viewport -> (Int, Int) -> Int -> Char -> Viewport
updateViewport viewport (x', y') lineQuantity direction
  | x' == (rows' - 2) && direction == 'j' && (rows' - 2) + 1 + initialRow' < lineQuantity = scrollDown viewport 1
  | x' == 0 && initialRow' > 0 && direction == 'k' = scrollUp viewport 1
  | y' == columns' = scrollRight viewport 1
  | y' == 0 && initialColumn' > 0 = scrollLeft viewport 1
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
