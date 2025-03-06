module Editor.FileManager where

import Editor.EditorState
  ( EditorState
      ( commandBuffer,
        extendedPieceTable,
        fileStatus,
        filename,
        mode,
        statusBar
      ),
    FileStatus (NotSaved, Saved),
    Mode (Normal, Closed),
  )
import Editor.ExtendedPieceTable
import Editor.StatusBar
import System.Directory (doesFileExist, getPermissions, renameFile, writable)
import System.Posix.User (getEffectiveUserID)

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

splitDirAndFileName :: String -> (String, String)
splitDirAndFileName path =
  let reversedPath = reverse path
      (reversedFilename, reversedDirname) = break (== '/') reversedPath
      newFilename = reverse reversedFilename
      newDirname = reverse reversedDirname
  in (newFilename, newDirname)

writeToFile :: EditorState -> FilePath -> IO EditorState
writeToFile state fname' = do
  let (fname, dname) = splitDirAndFileName fname'
      tempFile = dname ++ "." ++ fname ++ ".swp"
  writeFile tempFile (extendedPieceTableToString (extendedPieceTable state))
  renameFile tempFile fname'
  return
    state
      { mode = Normal,
        statusBar = clearError (statusBar state),
        filename = if null (filename state) then fname else filename state,
        fileStatus = Saved,
        commandBuffer = ""
      }

quitEditor :: EditorState -> IO EditorState
quitEditor state
  | fileStatus state == NotSaved = return $ setError state "No write since last change. Use \"w\" or \"q!\" to quit without saving."
  | otherwise = return state {mode = Closed}

saveAndQuit :: EditorState -> Bool -> [Char] -> IO EditorState
saveAndQuit state force args = do
  newState <- saveFile state force args

  case statusMode (statusBar newState) of
    Exception -> return newState
    NoException -> return newState {mode = Closed}

setError :: EditorState -> String -> EditorState
setError state msg = state {mode = Normal, statusBar = (statusBar state) {statusMode = Exception, errorMessage = msg}, commandBuffer = ""}

clearError :: StatusBar -> StatusBar
clearError sBar = sBar {statusMode = NoException, errorMessage = ""}
