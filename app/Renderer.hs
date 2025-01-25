module Renderer where


import System.IO(hFlush, stdout)
import System.Console.ANSI(setCursorPosition)
import Terminal.Render
import Editor.Viewport
import Editor.Cursor
import Editor.EditorState
import Editor.PieceTable


replaceChar :: String -> Char -> Char -> String -> String
replaceChar ""  _ _ acc = acc
replaceChar text targetChar changeChar acc = 
  if (head text) == targetChar then (replaceChar (tail text) targetChar changeChar (acc ++ [changeChar]))
  else (replaceChar (tail text) targetChar changeChar (acc ++ [(head text)]))

renderState :: EditorState -> IO ()
renderState (EditorState mode pieceTable cursor viewport) = do
  hideCursor
  clearScreen
  renderViewport pieceTable cursor viewport
  -- renderStatusBar mode
  renderCursor mode cursor
  showCursor
  hFlush stdout


renderViewport :: PieceTable -> Cursor -> Viewport -> IO ()
renderViewport pieceTable (Cursor x y) viewport = do
    let lines = (pieceTableToLineArray pieceTable)
        (_, ogBuffer, addBuffer, (iBuffer, startPos), sizesSeq) = pieceTable
    printLines lines viewport 0
    putStr ("Start pos: "++show startPos)
    putStr (" | IBuffer: "++(replaceChar iBuffer '\n' '⌣' "")++" |")
    putStr ("sizesSeq -> " ++ (show sizesSeq))
    putStr (" cursor: " ++ (show x) ++ " " ++ (show y))

renderCursor :: Mode -> Cursor -> IO ()
renderCursor mode (Cursor x y) = do
  case mode of
    Normal -> putStr "\ESC[1 q"
    Insert -> putStr "\ESC[5 q"
  setCursorPosition x y


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
  
    




