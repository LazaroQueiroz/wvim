module Main where

import Control.Monad(unless)
import System.IO
import System.Console.ANSI
import InputHandler
import Editor.EditorState
import Renderer
import Control.Concurrent (threadDelay)

-- | Read a character with a brief timeout for distinguishing ESC vs arrow keys
getCharRaw :: IO String
getCharRaw = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    first <- getChar
    if first == '\ESC' then do
        -- Wait 50ms: If another char arrives, it's part of an escape sequence
        threadDelay 5000  
        ready <- hReady stdin
        if ready then do
            second <- getChar
            if second == '[' then do
                third <- getChar
                return ['\ESC', '[', third] -- Arrow key sequence
            else return [first, second] -- Other ESC sequences
        else return [first] -- Treat as a single ESC press
    else return [first]



-- Main
main :: IO ()
main = do
  Just (width, height) <- getTerminalSize
  hSetBuffering stdin NoBuffering 
  hSetEcho stdin False
  clearScreen

  let defaultState = defaultEditorState width height-- abrir um arquivo e transformar ele numa PieceTable
  eventLoop defaultState

eventLoop :: EditorState -> IO ()
eventLoop editorState = do
  
  renderState editorState

  inputChar <- getCharRaw

  let newState = handleKeyPress editorState inputChar
  unless (not (isRunning editorState)) (eventLoop newState)

isRunning :: EditorState -> Bool
isRunning (EditorState mode pieceTable cursor viewPort) = 
    case mode of
        Closed -> False
        _ -> True
