module Renderer where


import System.IO(hFlush, stdout)
import System.Console.ANSI(setCursorPosition)
import Terminal.Render
import Editor.Viewport
import Editor.Cursor
import Editor.EditorState
import Editor.PieceTable



renderState :: EditorState -> IO ()
renderState (EditorState mode pieceTable cursor viewport) = do
  hideCursor
  renderViewport pieceTable viewport
  -- renderStatusBar mode
  renderCursor cursor
  showCursor
  hFlush stdout


renderViewport :: PieceTable -> Viewport -> IO ()
renderViewport pieceTable viewport = do
    let lines = (pieceTableToLineArray pieceTable)
        (_, ogBuffer, addBuffer, (iBuffer, startPos), sizesSeq) = pieceTable
    printLines lines viewport 0
    -- putStr ("Start pos: "++show startPos)
    -- putStr (" | IBuffer: "++iBuffer++" |")


renderCursor :: Cursor -> IO ()
renderCursor (Cursor x y) = setCursorPosition x y


printLines :: [String] -> Viewport -> Int -> IO ()
printLines lines (Viewport columns rows initialRow initialColumn) row = do
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
  
    




