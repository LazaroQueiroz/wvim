module Editor.CommandMode where

import Editor.EditorState (
  EditorState (
    extendedPieceTable,
    fileStatus,
    filename,
    mode,
    statusBar
  ),
  FileStatus (NotSaved, Saved),
  Mode (Normal),
 )
import Editor.ExtendedPieceTable
import Editor.StatusBar
import System.Directory (doesFileExist, getPermissions, renameFile, writable)
import System.Exit (exitSuccess)
import System.IO ()
import System.Posix.User (getEffectiveUserID)

handleCommandMode :: EditorState -> String -> IO EditorState
handleCommandMode state inputString =
  let (command, rawArgs) = break (== ' ') inputString
      args = dropWhile (== ' ') rawArgs
   in case command of
        "w" -> saveFile state False args
        "w!" -> saveFile state True args
        "q" -> quitEditor state
        "q!" -> exitSuccess
        "wq" -> saveAndQuit state False args
        "wq!" -> saveAndQuit state True args
        _ -> return $ setError state "Command not found."

saveFile :: EditorState -> Bool -> [Char] -> IO EditorState
saveFile state force args = do
  let fname = if null args then filename state else args
  if null fname
    then return $ setError state "File without name. Run \"w <filename>\"."
    else do
      fileExists <- doesFileExist fname
      if not fileExists
        then writeToFile state fname
        else do
          permissions <- getPermissions fname
          let canWrite = writable permissions
          uid <- getEffectiveUserID
          if not canWrite
            then
              if uid == 0
                then writeToFile state fname
                else
                  if force
                    then return $ setError state "Permission denied: Cannot write to file. Run editor with sudo to override."
                    else return $ setError state "Permission denied: Cannot write to file. Use \"w!\" to attempt force, or run with sudo."
            else writeToFile state fname

writeToFile :: EditorState -> FilePath -> IO EditorState
writeToFile state fname = do
  let tempFile = "." ++ fname ++ ".swp"
  writeFile tempFile (extendedPieceTableToString (extendedPieceTable state))
  renameFile tempFile fname
  return
    state
      { mode = Normal
      , statusBar = clearError (statusBar state)
      , filename = if null (filename state) then fname else filename state
      , fileStatus = Saved
      }

quitEditor :: EditorState -> IO EditorState
quitEditor state =
  if fileStatus state == NotSaved
    then
      return $ setError state "No write since last change. Use \"w\" or \"q!\" to quit without saving."
    else
      exitSuccess

saveAndQuit :: EditorState -> Bool -> [Char] -> IO EditorState
saveAndQuit state force args = do
  newState <- saveFile state force args
  if statusMode (statusBar newState) == Exception
    then
      return newState
    else
      exitSuccess

setError :: EditorState -> String -> EditorState
setError state msg =
  state{mode = Normal, statusBar = (statusBar state){statusMode = Exception, errorMessage = msg}}

clearError :: StatusBar -> StatusBar
clearError sBar = sBar{statusMode = NoException, errorMessage = ""}
