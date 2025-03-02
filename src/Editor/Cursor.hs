module Editor.Cursor where

import System.Console.ANSI ()
import System.IO ()
import Utils

-- Initial Cursor Position
data Cursor = Cursor {x :: Int, y :: Int} deriving (Show)

-- Update cursor position based on input
-- Pattern Matching: works like a switch case based on the inputs to the function.

updateCursor :: Char -> Cursor -> [Int] -> Cursor
updateCursor 'k' (Cursor cx cy) lineSizes =
  let newX = max 0 (cx - 1)
      newY = min (nth (newX + 1) lineSizes) cy
   in Cursor newX newY -- Move up
updateCursor 'j' (Cursor cx cy) lineSizes =
  let newX = min (length lineSizes - 1) (cx + 1)
      newY = min (nth (newX + 1) lineSizes) cy
   in Cursor newX newY -- Move down
updateCursor 'h' (Cursor cx cy) _ =
  let newX = cx
      newY = max 0 (cy - 1)
   in Cursor newX newY -- Move left
updateCursor 'l' (Cursor cx cy) lineSizes =
  let newX = cx
      newY = min (nth (cx + 1) lineSizes) (cy + 1)
   in Cursor newX newY -- Move right
updateCursor _ cursor _ = cursor -- No change

updateCursorPosition :: Cursor -> [Char] -> Int -> Cursor
updateCursorPosition (Cursor cx cy) "\DEL" aboveLineSize
  | cx == 0 = if cy == 0 then Cursor cx cy else Cursor cx (cy - 1)
  | cy == 0 = Cursor (cx - 1) aboveLineSize
  | otherwise = Cursor cx (cy - 1)
updateCursorPosition (Cursor cx _) "\n" _ =
  Cursor (cx + 1) 0
updateCursorPosition (Cursor cx cy) _ _ =
  Cursor cx (cy + 1)
