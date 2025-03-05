module Editor.Cursor where

import Editor.Viewport
import System.Console.ANSI ()
import System.IO ()
import Utils

-- Initial Cursor Position
data Cursor = Cursor {x :: Int, y :: Int} deriving (Show)

-- Updates cursor position based on user input
updateCursor :: Char -> Cursor ->  [Int] -> Bool -> Cursor
updateCursor input (Cursor x' y') linesSizes isInsertMode
  | input == 'k' -- Move up
    =
      let newX = max 0 (x' - 1)
          maxRight = maxY newX
          newY = min y' (maxRight + extra newX)
       in Cursor newX newY
  | input == 'j' -- Move down
    =
      let newX = min (length linesSizes - 1) (x' + 1)
          maxRight = maxY newX
          newY = min y' (maxRight + extra newX)

       in Cursor newX newY
  | input == 'h' -- Move left
    =
      let newX = x'
          newY = max 0 (y' - 1)
       in Cursor newX newY
  | input == 'l' -- Move right
    =
      let newX = x'
          maxRight = maxY newX
          newY = min (y' + 1) (maxRight + extra newX)
       in Cursor newX newY
  | otherwise = Cursor x' y' -- No change
  where
    maxY varX = max 0 ((linesSizes !! varX) - 1)
    extra varX
      | isInsertMode && not ((linesSizes !! varX) == 0) = 1
      | otherwise = 0

-- Updates cursor position after text modifications
updateCursorPosition :: Cursor -> [Char] -> Int -> Cursor
updateCursorPosition (Cursor x' y') input aboveLineSize
  | input == "\DEL" = handleDelete
  | input == "\n" = handleEnter
  | otherwise = Cursor x' (y' + 1)
  where
    handleEnter = Cursor (x' + 1) 0
    handleDelete
      | x' == 0 && y' == 0 = Cursor x' y'
      | y' == 0 = Cursor (x' - 1) aboveLineSize
      | otherwise = Cursor x' (y' - 1)

-- -- Updates cursor position based on user input
-- updateCursor :: Char -> Cursor -> Viewport -> [Int] -> Bool -> Cursor
-- updateCursor input (Cursor x' y') (Viewport rows' columns' initialRow' initialColumn') linesSizes isInsertMode
--   | input == 'k' -- Move up
--     =
--       let newX = max 0 (x' - 1)
--           maxRight = maxY newX
--           newY = max 0 (min (columns' - 1) (min (maxRight + extra - initialColumn') y'))
--        in Cursor newX newY
--   | input == 'j' -- Move down
--     =
--       let newX = min (rows' - 2) (min (length visibleLinesSizes - 1) (x' + 1))
--           maxRight = maxY newX
--           newY = max 0 (min (columns' - 1) (min (maxRight + extra - initialColumn') y'))
--        in Cursor newX newY
--   | input == 'h' -- Move left
--     =
--       let newX = x'
--           newY = max 0 (y' - 1)
--        in Cursor newX newY
--   | input == 'l' -- Move right
--     =
--       let newX = x'
--           maxRight = maxY newX
--           newY = max 0 (min (columns' - 1) (min (maxRight + extra - initialColumn') (y' + 1)))
--        in Cursor newX newY
--   | otherwise = Cursor x' y' -- No change
--   where
--     visibleLinesSizes = drop initialRow' linesSizes
--     maxY varX = nth (varX + 1) visibleLinesSizes - 1
--     extra
--       | isInsertMode = 1
--       | otherwise = 0
--
-- -- Updates cursor position after text modifications
-- updateCursorPosition :: Cursor -> Viewport -> [Char] -> Int -> Cursor
-- updateCursorPosition (Cursor x' y') viewport input aboveLineSize
--   | input == "\DEL" = handleDelete
--   | input == "\n" = handleEnter
--   | otherwise = Cursor x' (min (columns' - 1) (y' + 1))
--   where
--     (Viewport rows' columns' initialRow' initialColumn') = viewport
--     handleEnter
--       | x' == (rows' - 2) = Cursor x' 0
--       | otherwise = Cursor (x' + 1) 0
--     handleDelete
--       | x' == 0 && y' == 0 && initialRow' == 0 = Cursor x' y'
--       | y' == 0 && initialColumn' == 0 = Cursor (x' - 1) aboveLineSize
--       | otherwise = Cursor x' (y' - 1)
