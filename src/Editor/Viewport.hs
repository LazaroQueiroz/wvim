module Editor.Viewport where

data Viewport = Viewport
  { rows :: Int
  , columns :: Int
  , initialRow :: Int
  , initialColumn :: Int
  }
  deriving (Show, Eq)

defaultViewport :: Int -> Int -> Viewport
defaultViewport rows' columns' = Viewport rows' columns' 0 0

scrollUp :: Viewport -> Int -> Viewport
scrollUp viewport multiplier = viewport{initialRow = initialRow viewport - multiplier}

scrollDown :: Viewport -> Int -> Viewport
scrollDown viewport multiplier = viewport{initialRow = initialRow viewport + multiplier}

scrollLeft :: Viewport -> Int -> Viewport
scrollLeft viewport multiplier = viewport{initialRow = initialColumn viewport - multiplier}

scrollRight :: Viewport -> Int -> Viewport
scrollRight viewport multiplier = viewport{initialRow = initialColumn viewport + multiplier}

resizeViewport :: Viewport -> Int -> Int -> Viewport
resizeViewport viewport newRows newColumns = viewport{rows = newRows, columns = newColumns}
