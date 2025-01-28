module Editor.CommandMode where

import Editor.EditorState
import Editor.StatusBar
import Editor.ExtendedPieceTable
import System.Directory (renameFile, doesFileExist)
import System.IO (writeFile)
import System.FilePath.Find
import System.Exit (exitWith, ExitCode(ExitSuccess))

handleCommandMode :: EditorState -> String -> IO EditorState
handleCommandMode state inputString = do
  -- If the input is "w" or ":w"
  if (head inputString == 'w') then do
    -- Check if a filename is provided
    if (filename state == "") then do
      let sBar = statusBar state
          newStatusBar = sBar { statusMode = Exception, errorMessage = "File without name. Run \":w <filename>\"."}
          newState = state { mode = Normal, statusBar = newStatusBar }
      return newState
    else do
      -- Save the file
      let tempFile = "." ++ filename state ++ ".swp"
      writeFile tempFile (extendedPieceTableToString (extendedPieceTable state))
      renameFile tempFile (filename state)
      return state { mode = Normal }

  -- If the input is "q" or ":q"
  else if (inputString == "q") then do
    -- If the file is modified, prompt user or save automatically
    return state { mode = Normal }  -- You may want to check if the file has been modified or not before quitting
  -- Handle the case where ":wq" is used, save and quit
  else if (inputString == "wq") then do
    let tempFile = "." ++ filename state ++ ".swp"
    writeFile tempFile (extendedPieceTableToString (extendedPieceTable state))
    renameFile tempFile (filename state)
    exitWith ExitSuccess  -- Exit after saving
  -- Handle invalid commands
  else do
    let sBar = statusBar state
        newStatusBar = sBar { statusMode = Exception, errorMessage = "Command not found." }
    return state { mode = Normal, statusBar = newStatusBar }

-- Ensure to handle the default case for unsupported or invalid commands
handleCommandMode state _ = return state  -- Default for any invalid commands or unhandled inputs

