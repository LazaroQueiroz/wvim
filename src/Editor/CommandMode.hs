module Editor.CommandMode where

import Editor.EditorState
import Editor.StatusBar
import Editor.ExtendedPieceTable
import System.Directory (doesFileExist, writable, renameFile, getPermissions)
import System.IO (writeFile)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.Posix.User (getEffectiveUserID)

handleCommandMode :: EditorState -> String -> IO EditorState
handleCommandMode state inputString
  | inputString == "w" || inputString == ":w" = saveFile state False
  | inputString == "w!" || inputString == ":w!" = saveFile state True
  | inputString == "q" || inputString == ":q" = quitEditor state
  | inputString == "q!" || inputString == ":q!" = exitWith ExitSuccess
  | inputString == "wq" || inputString == ":wq" = saveAndQuit state False
  | inputString == "wq!" || inputString == ":wq!" = saveAndQuit state True
  | otherwise = return $ setError state "Command not found."

saveFile :: EditorState -> Bool -> IO EditorState
saveFile state force = do
    permissions <- getPermissions (filename state)
    let canWrite = writable permissions
        fname = (filename state)
    if null fname then
        return $ setError state "File without name. Run \"w <filename>\"."
    else do
        uid <- getEffectiveUserID
        if not canWrite then
            if uid == 0 then
                writeToFile state fname
            else if force then
                return $ setError state "Permission denied: Cannot write to file. Run editor with sudo to override."
            else
                return $ setError state "Permission denied: Cannot write to file. Use \"w!\" to attempt force, or run with sudo."
        else
            writeToFile state fname

writeToFile :: EditorState -> FilePath -> IO EditorState
writeToFile state fname = do
    let tempFile = "." ++ fname ++ ".swp"
    writeFile tempFile (extendedPieceTableToString (extendedPieceTable state))
    renameFile tempFile fname
    return state { mode = Normal, statusBar = clearError (statusBar state), fileStatus = Saved }

quitEditor :: EditorState -> IO EditorState
quitEditor state =
    if fileStatus state == NotSaved then
        return $ setError state "No write since last change. Use \"w\" or \"q!\" to quit without saving."
    else
        exitWith ExitSuccess

saveAndQuit :: EditorState -> Bool -> IO EditorState
saveAndQuit state force = do
    newState <- saveFile state force
    if statusMode (statusBar newState) == Exception then
        return newState  -- Don't quit if save failed
    else
        exitWith ExitSuccess

setError :: EditorState -> String -> EditorState
setError state msg =
    state { mode = Normal, statusBar = (statusBar state) { statusMode = Exception, errorMessage = msg } }

clearError :: StatusBar -> StatusBar
clearError sBar = sBar { statusMode = NoException, errorMessage = "" }

