module Renderer where

import System.IO(hFlush, stdout)
import System.Console.ANSI(setCursorPosition)
import Terminal.Render
import Editor.Viewport
import Editor.Cursor
import Editor.EditorState
import Editor.ExtendedPieceTable
import System.Directory 

import Utils


-- Renders the current state of the editor: the Viewport (actual content of the file), the status bar (which contains essential information about the editor state and the file) and renders the correct state of the cursor (position and style).
-- @param editorState :: EditorState -> current state of the editor.
renderState :: EditorState -> IO ()
renderState (EditorState mode extendedPieceTable cursor viewport filename) = do
  clearScreen
  renderViewport extendedPieceTable cursor viewport filename
  -- renderStatusBar mode TODO: implement the rendering of the status bar.
  renderCursor mode cursor


-- Renders the viewport, meaning that it renders all the contents of the files given the current dimensions of the viewport.
-- @param pieceTable :: PieceTable -> 
renderViewport :: ExtendedPieceTable -> Cursor -> Viewport -> String -> IO ()
renderViewport extendedPieceTable (Cursor x y) viewport filename = do
    let lines = (extendedPieceTableToLineArray extendedPieceTable)
        (_, ogBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = extendedPieceTable
    printLines lines viewport 0
    putStr ("Start pos: " ++ (show insertStartIndex))
    putStr (" | IBuffer: " ++ (replace (replace insertBuffer '\n' '⌣' "") '\DEL' '*' ""))
    putStr ("sizesSeq: " ++ (show linesSizes))
    putStr ("| cursor: " ++ (show x) ++ " " ++ (show y))
    dir <- getCurrentDirectory
    putStr ("| dir: " ++ dir ++ filename)
    hFlush stdout

-- Renders the cursor in the terminal based on its position and style.
-- @param mode :: Mode -
renderCursor :: Mode -> Cursor -> IO ()
renderCursor curMode (Cursor x y) = do
  setCursorPosition x y
  case curMode of
    Normal -> putStr "\ESC[1 q"
    Insert -> putStr "\ESC[5 q"
  hFlush stdout


printLines :: [String] -> Viewport -> Int -> IO ()
printLines lines (Viewport columns rows initialRow initialColumn) row = do
    hideCursor
    let viewport = (Viewport columns rows initialRow initialColumn)
    if row == (rows - 1) then
          putStr ""
    else do
          if lines == [] then do
              putStrLn "~"
              printLines lines viewport (row + 1)
          else do
              moveCursor (Cursor 0 row) 
              putStrLn (head lines)
              printLines (tail lines) viewport (row + 1)
    showCursor
  
    




