module Editor.Cursor where

import System.IO
import System.Console.ANSI

-- Initial Cursor Position
data Cursor = Cursor { x :: Int, y :: Int } deriving Show 
-- data Grid = Grid { grid :: [[Char]] } deriving Show 

-- Update cursor position based on input
-- Pattern Matching: works like a switch case based on the inputs to the function.
updateCursor :: Char -> Cursor -> Cursor
updateCursor 'k' (Cursor x y) = Cursor (max 0 (x - 1)) y  -- Move up
updateCursor 'j' (Cursor x y) = Cursor (x + 1) y          -- Move down
updateCursor 'h' (Cursor x y) = Cursor x (max 0 (y - 1))  -- Move left
updateCursor 'l' (Cursor x y) = Cursor x (y + 1)          -- Move right
updateCursor _ cursor = cursor                            -- No change
