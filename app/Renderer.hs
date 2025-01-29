module Renderer where

import System.IO(hFlush, stdout)
import System.Console.ANSI(setCursorPosition)
import Terminal.Render
import Editor.Viewport
import Editor.Cursor
import Editor.EditorState
import Editor.ExtendedPieceTable
import Editor.StatusBar
import System.Directory 

import Utils


-- Renders the current state of the editor: the Viewport (actual content of the file), the status bar (which contains essential information about the editor state and the file) and renders the correct state of the cursor (position and style).
-- @param editorState :: EditorState -> current state of the editor.
renderState :: EditorState -> IO ()
renderState (EditorState mode extendedPieceTable cursor viewport fileStatus filename statusBar) = do
  clearScreen
  renderViewport extendedPieceTable cursor viewport filename
  renderStatusBar mode viewport cursor filename (statusMode statusBar) (errorMessage statusBar)
  renderCursor mode cursor


-- Renders the viewport, meaning that it renders all the contents of the files given the current dimensions of the viewport.
-- @param pieceTable :: PieceTable -> 
renderViewport :: ExtendedPieceTable -> Cursor -> Viewport -> String -> IO ()
renderViewport extendedPieceTable (Cursor x y) viewport filename = do
    let lines = (extendedPieceTableToLineArray extendedPieceTable)
        (_, ogBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = extendedPieceTable
    printLines lines viewport 0

renderStatusBar :: Mode -> Viewport -> Cursor -> String -> StatusMode -> String -> IO ()
renderStatusBar mode viewport cursor filename sBarMode errorMsg = do
    moveCursor (Cursor 0 (rows viewport))
    case sBarMode of
      NoException -> do
        putStr ((showMode mode) ++ " | ")
        putStr ("Path: " ++ filename ++ " | ")
      otherwise -> do
        putStr (errorMsg)
    putStr (show ((x cursor) + 1) ++ ", " ++ (show ((y cursor) + 1)) ++ " |")
    putStr (" " ++ (getLineProgress viewport cursor))


showMode :: Mode -> String
showMode mode =
  case mode of
    Normal -> "Normal"
    Insert -> "Insert"
    Command -> "Command"

getLineProgress :: Viewport -> Cursor -> String
getLineProgress viewport cursor = 
  let coverPercentage = ((x cursor) `div` (rows viewport)) * 100
      progress = 
        if (coverPercentage == 100) then "Bot"
        else if (coverPercentage == 0) then "Top"
        else (show coverPercentage) ++ "%"
  in progress

  



-- Renders the cursor in the terminal based on its position and style.
-- @param mode :: Mode -
renderCursor :: Mode -> Cursor -> IO ()
renderCursor curMode (Cursor x y) = do
  setCursorPosition x y
  case curMode of
    Normal -> putStr "\ESC[1 q"
    Insert -> putStr "\ESC[5 q"
    Command -> putStr "\ESC[5 q"
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
  
    




