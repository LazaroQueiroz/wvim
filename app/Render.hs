module Render where

import Editor.Cursor
import System.Console.ANSI
import System.IO

clearScreen :: IO ()
clearScreen = System.Console.ANSI.clearScreen

-- Move the cursor in the terminal
moveCursor :: Cursor -> IO () -- type declaration
moveCursor (Cursor x' y') = setCursorPosition y' x' -- set cursor position comes from the library (Console.ANSI)

hideCursor :: IO ()
hideCursor = do
  putStr "\ESC[?25l"
  hFlush stdout

showCursor :: IO ()
showCursor = do
  putStr "\ESC[?25h"
  hFlush stdout

setTextColor :: String -> IO ()
setTextColor color = do
  putStr $ case color of
    "red" -> "\ESC[31m"
    "green" -> "\ESC[32m"
    "yellow" -> "\ESC[33m"
    "blue" -> "\ESC[34m"
    "reset" -> "\ESC[0m"
    _ -> "\ESC[0m"
  hFlush stdout
