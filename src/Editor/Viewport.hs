module Editor.Viewport where

data Viewport = Viewport
  { rows :: Int,
    columns :: Int,
    initialRow :: Int,
    initialColumn :: Int
  }
  deriving (Show, Eq)


updateViewport :: Viewport -> (Int, Int) -> Viewport
updateViewport viewport (x', y')
  | x' < initialRow' = scrollUp viewport (initialRow' - x')
  | (x' - initialRow') > (rows' - 2) = scrollDown viewport (x' - initialRow' - rows' + 2)
  | y' < initialColumn' = scrollLeft viewport (initialColumn' - y')
  | (y' - initialColumn') > (columns' - 1) = scrollRight viewport (y' - initialColumn' - columns' + 1) 
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
