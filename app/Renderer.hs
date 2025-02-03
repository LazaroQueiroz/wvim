{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Renderer where

import Editor.Cursor
import Editor.EditorState
import Editor.ExtendedPieceTable
import Editor.StatusBar
import Editor.Viewport
import System.Console.ANSI (setCursorPosition)
import System.IO (hFlush, stdout)
import Terminal.Render

-- Renders the current state of the editor: the Viewport (actual content of the file), the status bar (which contains essential information about the editor state and the file) and renders the correct state of the cursor (position and style).
-- @param editorState :: EditorState -> current state of the editor.
renderState :: EditorState -> IO ()
renderState (EditorState mode' extendedPieceTable' cursor' viewport' _ filename' statusBar' commandText') = do
  clearScreen
  renderViewport extendedPieceTable' cursor' viewport' filename'
  renderStatusBar mode' viewport' cursor' filename' (statusMode statusBar') (errorMessage statusBar') commandText' extendedPieceTable' 
  renderCursor mode' cursor'

-- Renders the viewport, meaning that it renders all the contents of the files given the current dimensions of the viewport.
renderViewport :: ExtendedPieceTable -> Cursor -> Viewport -> String -> IO ()
renderViewport extendedPieceTable' _ viewport' _ = do
  let lines' = extendedPieceTableToLineArray extendedPieceTable'
  printLines lines' viewport' 0
  hFlush stdout

renderStatusBar :: Mode -> Viewport -> Cursor -> String -> StatusMode -> String -> String -> ExtendedPieceTable -> IO ()
renderStatusBar mode' viewport' cursor' filename' sBarMode errorMsg commandText' extendedPieceTable' = do
  moveCursor (Cursor 0 (rows viewport'))
  putStr $ "| " ++ showMode mode' ++ " | "
  case sBarMode of
      NoException -> do
        putStr $ "Path: " ++ shownFileName ++ " | "
      Exception -> do
        putStr $ errorMsg ++ " | "
  case mode' of
    Command -> do
      putStr $ ":" ++ commandText'
    Normal -> do
      putStr $ show (x cursor' + 1) ++ ", " ++ show (y cursor' + 1) ++ " | "
      putStr $ show (rows viewport') ++ "x" ++ show (columns viewport') ++ " | "
      putStr $ getLineProgress extendedPieceTable' cursor' ++ " | "
    Insert -> do
      putStr $ show (x cursor' + 1) ++ ", " ++ show (y cursor' + 1) ++ " | "
      putStr $ "sizes:" ++ show linesSizes ++ " | stidx:" ++ show insertStartIndex ++ " | "
      putStr $ "iBuf:" ++ insertBuffer ++ " | "
      --putStr $ "oBuf:" ++ show originalBuffer ++ " | " 
      --putStr $ "aBuf:" ++ show addBuffer ++ " | "
      --putStr $ show (piecesCollToString pieces)
  where
    (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = extendedPieceTable'
    shownFileName
      | null filename' = "None"
      | otherwise = filename'

showMode :: Mode -> String
showMode mode' =
  case mode' of
    Normal -> "Normal"
    Insert -> "Insert"
    Command -> "Command"

getLineProgress :: ExtendedPieceTable -> Cursor -> String
getLineProgress (_, _, _, _, _, linesSizes) cursor'
  | x cursor' == 0 = "Top"
  | x cursor' == length linesSizes - 1 = "Bot"
  | otherwise = show ((x cursor' + 1) * 100 `div` length linesSizes) ++ "%"

-- Renders the cursor in the terminal based on its position and style.
renderCursor :: Mode -> Cursor -> IO ()
renderCursor curMode (Cursor x' y') = do
  setCursorPosition x' y'
  case curMode of
    Normal -> putStr "\ESC[1 q"
    Insert -> putStr "\ESC[5 q"
    Command -> putStr "\ESC[5 q"
  hFlush stdout

printLines :: [String] -> Viewport -> Int -> IO ()
printLines lines' (Viewport rows' columns' initialRow' initialColumn') row
  | row == (rows' - 1) 
    = 
      --hideCursor
      putStr ""
      --showCursor
  | null lines'
    = do
      --hideCursor
      putStrLn "~"
      printLines lines' viewport' (row + 1)
      --showCursor
  | otherwise 
    = do
      --hideCursor
      moveCursor (Cursor 0 row)
      putStrLn (head lines')
      printLines (tail lines') viewport' (row + 1)
      --showCursor
  where
    viewport' = Viewport rows' columns' initialRow' initialColumn'