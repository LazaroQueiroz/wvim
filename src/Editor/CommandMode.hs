module Editor.CommandMode where

import Editor.EditorState
import System.Exit (exitSuccess)
import System.IO ()
import Editor.FileManager

-- Handles user input in Command mode.
handleCommandMode :: EditorState -> String -> IO EditorState
handleCommandMode state inputString
  | command == "w"   = saveFile state False args
  | command == "w!"  = saveFile state True args
  | command == "q"   = quitEditor state
  | command == "q!"  = exitSuccess
  | command == "wq"  = saveAndQuit state False args
  | command == "wq!" = saveAndQuit state True args
  | otherwise        = return $ setError state "Command not found."
  where
    (command, rawArgs) = break (== ' ') inputString
    args = dropWhile (== ' ') rawArgs