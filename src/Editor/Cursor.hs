module Editor.Cursor where

import System.Console.ANSI ()
import System.IO ()
import Utils

-- Initial Cursor Position
data Cursor = Cursor {x :: Int, y :: Int} deriving (Show)

-- Update cursor position based on input
-- Pattern Matching: works like a switch case based on the inputs to the function.

updateCursor :: Char -> Cursor -> [Int] -> Bool -> Cursor
updateCursor 'k' (Cursor x' y') lineSizes _ =
  let newX = max 0 (x' - 1)
      newY = min (nth (newX + 1) lineSizes) y'
   in Cursor newX newY -- Move up
updateCursor 'j' (Cursor x' y') lineSizes _ =
  let newX = min (length lineSizes - 1) (x' + 1)
      newY = min (nth (newX + 1) lineSizes) y'
   in Cursor newX newY -- Move down
updateCursor 'h' (Cursor x' y') _ _ =
  let newX = x'
      newY = max 0 (y' - 1)
   in Cursor newX newY -- Move left
updateCursor 'l' (Cursor x' y') lineSizes isInsertMode =
  let maxRight = nth (x' + 1) lineSizes - 1
      extra = if isInsertMode then 1 else 0 -- Permite ir um a mais se estiver no modo Insert
      newX = x'
      newY = min (maxRight + extra) (y' + 1)
   in Cursor newX newY -- Move right
updateCursor _ cursor _ _ = cursor -- No change

updateCursorPosition :: Cursor -> [Char] -> Int -> Cursor
updateCursorPosition (Cursor x' y') "\DEL" aboveLineSize
  | x' == 0 = if y' == 0 then Cursor x' y' else Cursor x' (y' - 1)
  | y' == 0 = Cursor (x' - 1) aboveLineSize
  | otherwise = Cursor x' (y' - 1)
updateCursorPosition (Cursor x' _) "\n" _ =
  Cursor (x' + 1) 0
updateCursorPosition (Cursor x' y') _ _ =
  Cursor x' (y' + 1)
