import System.IO
import System.Console.ANSI

-- Initial Cursor Position
data Cursor = Cursor { x :: Int, y :: Int } deriving Show

-- Move the cursor in the terminal
moveCursor :: Cursor -> IO ()
moveCursor (Cursor x y) = setCursorPosition y x

-- Update cursor position based on input
updateCursor :: Char -> Cursor -> Cursor
updateCursor 'w' (Cursor x y) = Cursor x (max 0 (y - 1))  -- Move up
updateCursor 's' (Cursor x y) = Cursor x (y + 1)          -- Move down
updateCursor 'a' (Cursor x y) = Cursor (max 0 (x - 1)) y  -- Move left
updateCursor 'd' (Cursor x y) = Cursor (x + 1) y          -- Move right
updateCursor _ cursor = cursor                            -- No change

-- Main loop for controlling the cursor
cursorControlLoop :: Cursor -> IO ()
cursorControlLoop cursor = do
  moveCursor cursor
  hFlush stdout
  char <- getChar
  if char == 'q' 
    then clearScreen
    else cursorControlLoop (updateCursor char cursor)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clearScreen
  putStrLn "Use W, A, S, D to move the cursor. Press 'q' to quit."
  cursorControlLoop (Cursor 0 0)

