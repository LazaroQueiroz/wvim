module Main where

import Control.Monad(unless)
import System.IO
import System.Console.ANSI
import InputHandler
import Editor.EditorState
import Renderer

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

  inputChar <- getChar

  let newState = handleKeyPress editorState inputChar
  unless (not (isRunning editorState)) (eventLoop newState)

isRunning :: EditorState -> Bool
isRunning (EditorState mode pieceTable cursor viewPort) = 
    case mode of
        Closed -> False
        _ -> True
