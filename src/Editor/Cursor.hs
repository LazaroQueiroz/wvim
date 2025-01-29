module Editor.Cursor where

import System.IO
import System.Console.ANSI
import Utils

-- Initial Cursor Position
data Cursor = Cursor { x :: Int, y :: Int } deriving Show 
-- data Grid = Grid { grid :: [[Char]] } deriving Show 

-- Update cursor position based on input
-- Pattern Matching: works like a switch case based on the inputs to the function.
updateCursor :: Char -> Cursor -> [Int] -> Cursor
updateCursor 'k' (Cursor x y) lineSizes = 
  let newX = (max 0 (x - 1))
      newY = (min ((nth (newX + 1) lineSizes) - 1) y)
  in Cursor newX newY         -- Move up
updateCursor 'j' (Cursor x y) lineSizes = 
  let newX = (min ((length lineSizes) - 1) (x + 1))
      newY = (min (nth (newX + 1) lineSizes) y)
  in Cursor newX newY         -- Move down
updateCursor 'h' (Cursor x y) lineSizes =
  let newX = x
      newY = (max 0 (y - 1))
  in Cursor newX newY         -- Move left
updateCursor 'l' (Cursor x y) lineSizes =
  let newX = x
      newY = (min ((nth (x + 1) lineSizes)) (y + 1))
  in Cursor newX newY         -- Move right
updateCursor _ cursor lineSizes = cursor                           -- No change


updateCursorPosition :: Cursor -> [Char] -> Int -> Cursor
updateCursorPosition (Cursor x y) "\DEL" aboveLineSize =
  if (x == 0) then
    if (y == 0) then (Cursor x y)
    else (Cursor x (y - 1))
  else
    if (y == 0) then (Cursor (x - 1) aboveLineSize)
    else (Cursor x (y - 1))
updateCursorPosition (Cursor x y) "\n" _ =
  (Cursor (x + 1) 0)
updateCursorPosition (Cursor x y) _ _ =
  (Cursor x (y + 1))

