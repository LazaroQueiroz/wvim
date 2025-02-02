module Editor.Cursor where

import System.Console.ANSI ()
import System.IO ()
import Utils

-- Initial Cursor Position
data Cursor = Cursor {x :: Int, y :: Int} deriving (Show)

-- Updates cursor position based on user input
updateCursor :: Char -> Cursor -> [Int] -> Bool -> Cursor
updateCursor input (Cursor x' y') lineSizes isInsertMode
  | input == 'k' -- Move up
    =
      let newX = max 0 (x' - 1)
          maxRight = maxY newX
          newY = max 0 (min (maxRight + extra) y')
       in Cursor newX newY
  | input == 'j' -- Move down
    =
      let newX = min (length lineSizes - 1) (x' + 1)
          maxRight = maxY newX
          newY = max 0 (min (maxRight + extra) y')
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
          newY = max 0 (min (maxRight + extra) (y' + 1))
       in Cursor newX newY
  | otherwise = Cursor x' y' -- No change
  where
    maxY varX = nth (varX + 1) lineSizes - 1
    extra
      | isInsertMode = 1
      | otherwise = 0

-- Updates cursor position after text modifications
updateCursorPosition :: Cursor -> [Char] -> Int -> Cursor
updateCursorPosition (Cursor x' y') input aboveLineSize
  | input == "\DEL" = handleDelete
  | input == "\n" = Cursor (x' + 1) 0
  | otherwise = Cursor x' (y' + 1)
  where
    handleDelete
      | x' == 0 && y' == 0  = Cursor x' y'
      | y' == 0 = Cursor (x' - 1) aboveLineSize
      | otherwise = Cursor x' (y' - 1)
