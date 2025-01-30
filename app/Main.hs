module Main where

import Control.Monad(unless)
import System.IO
import System.Console.ANSI
import InputHandler
import Editor.EditorState
import Renderer
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import Editor.FileManager
import System.Directory (doesFileExist)
import Editor.CommandMode

-- Read a character with a brief timeout for distinguishing ESC vs arrow keys
-- @return String that represents the character (composite or individual) received from the user.
getCharRaw :: IO String
getCharRaw = do
    firstChar <- getChar
    if firstChar == '\ESC' then do
        threadDelay 5000  
        isCharInHandleBuffer <- hReady stdin
        if isCharInHandleBuffer then do
            secondChar <- getChar
            if secondChar == '[' then do
                thirdChar <- getChar
                return ['\ESC', '[', thirdChar]
            else return ['\ESC', secondChar] 
        else return ['\ESC'] 
    else return [firstChar]

-- Sets the terminal configurations: disable input buffering and input echoing
setTerminalConfiguration :: IO ()
setTerminalConfiguration = do
  
  hSetBuffering stdin NoBuffering 
  hSetEcho stdin False
  clearScreen

-- Sets the terminal configurations to disable input character buffering and input echoing (writing the input in the terminal as it is received) and start the main event loop.
main :: IO ()
main = do

  args <- getArgs

  Just (width, height) <- getTerminalSize
  setTerminalConfiguration

  startingState <- case args of
      [] -> return (defaultEditorState width height "")  -- Default editor state if no args
      [nameOfTheFile] -> do
        exists <- doesFileExist nameOfTheFile  -- Check if file exists
        if exists then do
          file <- readFile nameOfTheFile  -- Read the file content
          return (editorStateFromFile file width height nameOfTheFile)  -- Create EditorState from file
        else do
          return (defaultEditorState width height nameOfTheFile)  -- Return default state if file doesn't exist
      _ -> return (defaultEditorState width height "")  -- Fallback for extra arguments, use default state

  eventLoop startingState

-- Mantain the main recursion loop running. Based on the current editor state, it renders this state, process the user input and then process the new state based on it.
-- @param editorState :: EditorState - current state of the editor.
eventLoop :: EditorState -> IO ()
eventLoop editorState = do

  renderState editorState

  let inputFunction = if (mode editorState) == Command then getLine else getCharRaw

  inputString <- inputFunction


  newState <- handleKeyPress editorState inputString
              
  unless (not (isRunning editorState)) (eventLoop newState)

-- Verifies if the current editor state is a valid (or running) state. If this is the case, return True, otherwise, False.
-- @param editorState :: EditorState - current state of the editor.
isRunning :: EditorState -> Bool
isRunning (EditorState mode pieceTable cursor viewPort fileStatus filename statusBar) = 
    case mode of
        Closed -> False
        _ -> True
