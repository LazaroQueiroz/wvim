module Editor.Cursor where

import System.Console.ANSI ()
import System.IO ()
import Utils

-- Initial Cursor Position
data Cursor = Cursor {x :: Int, y :: Int} deriving (Show)

-- Update cursor position based on input
updateCursor :: Char -> Cursor -> [Int] -> Bool -> Cursor
updateCursor input (Cursor x' y') lineSizes isInsertMode
  | input == 'k' -- Move up
    =
      let newX = max 0 (x' - 1)
          maxRight = nth (newX + 1) lineSizes - 1
          extra = if isInsertMode then 1 else 0
          newY = if maxRight == -1 then 0 else min (maxRight + extra) y'
       in Cursor newX newY
  | input == 'j' -- Move down
    =
      let newX = min (length lineSizes - 1) (x' + 1)
          maxRight = nth (newX + 1) lineSizes - 1
          extra = if isInsertMode then 1 else 0
          newY = if maxRight == -1 then 0 else min (maxRight + extra) y'
       in Cursor newX newY
  | input == 'h' -- Move left
    =
      let newX = x'
          newY = max 0 (y' - 1)
       in Cursor newX newY
  | input == 'l' -- Move right
    =
      let maxRight = nth (x' + 1) lineSizes - 1
          extra = if isInsertMode then 1 else 0
          newX = x'
          newY = if maxRight == -1 then 0 else min (maxRight + extra) (y' + 1)
       in Cursor newX newY
  | otherwise -- No change
    =
      Cursor x' y'

updateCursorPosition :: Cursor -> [Char] -> Int -> Cursor
updateCursorPosition (Cursor x' y') "\DEL" aboveLineSize
  | x' == 0 = if y' == 0 then Cursor x' y' else Cursor x' (y' - 1)
  | y' == 0 = Cursor (x' - 1) aboveLineSize
  | otherwise = Cursor x' (y' - 1)
updateCursorPosition (Cursor x' _) "\n" _ =
  Cursor (x' + 1) 0
updateCursorPosition (Cursor x' y') _ _ =
  Cursor x' (y' + 1)
