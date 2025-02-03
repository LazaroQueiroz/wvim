module Editor.CommandMode where

import Editor.EditorState
import System.Exit (exitSuccess)
import System.IO ()
import Editor.FileManager

-- Handles user input in Command mode.
handleCommandMode :: EditorState -> String -> IO EditorState
handleCommandMode state inputChar
  | inputChar == "\ESC" = return state {mode = Normal, commandText = ""}
  | inputChar == "\DEL" = handleDelete
  | inputChar == "\n" = processCommand state (commandText state)
  | otherwise = return $ state { commandText = commandText state ++ inputChar }
  where
    handleDelete
     | null (commandText state) = return state
     | otherwise = return $ state { commandText = init (commandText state) }

-- Process the command when the user presses "Enter"
processCommand :: EditorState -> String -> IO EditorState
processCommand state inputString
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