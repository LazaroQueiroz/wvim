module Terminal.Render where

import System.IO
import System.Console.ANSI
import Editor.Cursor


clearScreen :: IO ()
clearScreen = System.Console.ANSI.clearScreen

-- Move the cursor in the terminal
moveCursor :: Cursor -> IO () -- type declaration
moveCursor (Cursor x y) = setCursorPosition y x -- set cursor position comes from the library (Console.ANSI)

hideCursor :: IO ()
hideCursor = putStr "\ESC[?25l"

showCursor :: IO ()
showCursor = putStr "\ESC[?25h"

setTextColor :: String -> IO ()
setTextColor color =
    putStr $ case color of
        "red"    -> "\ESC[31m"
        "green"  -> "\ESC[32m"
        "yellow" -> "\ESC[33m"
        "blue"   -> "\ESC[34m"
        "reset"  -> "\ESC[0m"
        _        -> "\ESC[0m"




