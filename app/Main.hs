module Main where

import Control.Concurrent (threadDelay)
import Editor.EditorState
import InputHandler
import Renderer
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO

-- Read a character with a brief timeout for distinguishing ESC vs arrow keys
-- @return String that represents the character (composite or individual) received from the user.
getCharRaw :: IO String
getCharRaw = do
  char <- getChar  
  threadDelay 2000 --TODO: Botar a condição do "\ESC" de volta pq se vc esmagar todas as teclas do teclado rapidamente ele vai inserir letras demais
  isCharInHandleBuffer <- hReady stdin  
  if isCharInHandleBuffer
    then getChars [char]  
    else return [char]  

-- Função auxiliar que captura uma sequência de caracteres
getChars :: [Char] -> IO String
getChars charArray = do
  isCharInHandleBuffer <- hReady stdin
  if isCharInHandleBuffer
    then do
      char <- getChar  
      getChars (charArray ++ [char]) --Funciona, agora vamos ter que resolver as coisas no inputHandler ;-;
    else return charArray

-- Sets the terminal configurations: disable input buffering and input echoing
setTerminalConfiguration :: IO ()
setTerminalConfiguration = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clearScreen

-- Sets the terminal configurations to disable input character buffering and input echoing (writing the input in the terminal as it is received) and start the main event loop.
-- TODO aplicar functors para simplificar os ifs
main :: IO ()
main = do
  args <- getArgs
  Just (rows, columns) <- getTerminalSize
  setTerminalConfiguration

  startingState <- case args of
    [] -> return (defaultEditorState rows columns "") -- Default editor state if no args
    [nameOfTheFile] -> do
      exists <- doesFileExist nameOfTheFile -- Check if file exists
      if exists
        then do
          file <- readFile nameOfTheFile -- Read the file content
          return (editorStateFromFile file rows columns nameOfTheFile) -- Create EditorState from file
        else do
          return (defaultEditorState rows columns nameOfTheFile) -- Return default state if file doesn't exist
    _ -> return (defaultEditorState rows columns "") -- Fallback for extra arguments, use default state
  eventLoop startingState

-- Mantain the main recursion loop running. Based on the current editor state, it renders this state, process the user input and then process the new state based on it.
-- @param editorState :: EditorState - current state of the editor.
eventLoop :: EditorState -> IO ()
eventLoop = unfoldM step
 where
  step editorState = do
    renderState editorState
    inputString <- getCharRaw
    newState <- handleKeyPress editorState inputString
    return $ if isRunning newState then Just newState else Nothing

unfoldM :: (a -> IO (Maybe a)) -> a -> IO ()
unfoldM f a = do
  result <- f a
  maybe (return ()) (unfoldM f) result

-- Verifies if the current editor state is a valid (or running) state. If this is the case, return True, otherwise, False.
-- @param editorState :: EditorState - current state of the editor.
isRunning :: EditorState -> Bool
isRunning (EditorState Closed _ _ _ _ _ _ _) = False
isRunning _ = True
