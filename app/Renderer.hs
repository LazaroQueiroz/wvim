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

-- Renders the status bar with mode, cursor position, file info, and errors.
renderStatusBar :: Mode -> Viewport -> Cursor -> String -> StatusMode -> String -> String -> ExtendedPieceTable -> IO ()
renderStatusBar mode' viewport' cursor' filename' sBarMode errorMsg commandText' extendedPieceTable' = do
  moveCursor (Cursor 0 (rows viewport'))
  putStr $ "| " ++ showMode ++ " | "
  putStr $ showStatusBar ++ " | "
  case mode' of
    Command -> do
      putStr $ ":" ++ commandText'
    Normal -> do
      putStr $ show (x cursor' + 1) ++ ", " ++ show (y cursor' + 1) ++ " | "
      putStr $ show (rows viewport') ++ "x" ++ show (columns viewport') ++ " | "
      putStr $ getLineProgress extendedPieceTable' cursor' ++ " | "
    Visual -> do
      putStr $ show (x cursor' + 1) ++ ", " ++ show (y cursor' + 1) ++ " | "
      putStr $ show (rows viewport') ++ "x" ++ show (columns viewport') ++ " | "
      putStr $ getLineProgress extendedPieceTable' cursor' ++ " | "
    Insert -> do
      putStr $ show (x cursor' + 1) ++ ", " ++ show (y cursor' + 1) ++ " | "
      putStr $ "sizes:" ++ show linesSizes ++ " | stidx:" ++ show insertStartIndex ++ " | "
      --putStr $ "iBuf:" ++ insertBuffer ++ " | "
      --putStr $ "oBuf:" ++ show originalBuffer ++ " | " 
      --putStr $ "aBuf:" ++ show addBuffer ++ " | "
      --putStr $ show (piecesCollToString pieces)
    Replace -> do
      putStr $ show (x cursor' + 1) ++ ", " ++ show (y cursor' + 1) ++ " | "
      putStr $ "sizes:" ++ show linesSizes ++ " | stidx:" ++ show insertStartIndex ++ " | "
      --putStr $ "iBuf:" ++ insertBuffer ++ " | "
      --putStr $ "oBuf:" ++ show originalBuffer ++ " | " 
      --putStr $ "aBuf:" ++ show addBuffer ++ " | "
      --putStr $ show (piecesCollToString pieces)
  where
    (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = extendedPieceTable'
    
    shownFileName
      | null filename' = "None"
      | otherwise = filename'
    
    showMode =
      case mode' of
      Normal -> "Normal"
      Insert -> "Insert"
      Command -> "Command"
      Replace -> "Replace"
      Visual -> "Visual"

    showStatusBar =
      case sBarMode of
      NoException -> "Path: " ++ shownFileName
      Exception -> errorMsg 

-- Returns the cursor's vertical position as "Top", "Bot", or a percentage.
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
    Visual -> putStr "\ESC[1 q"
    Replace -> putStr "\ESC[1 q"
    Insert -> putStr "\ESC[5 q"
    Command -> putStr "\ESC[5 q"
  hFlush stdout

-- Renders lines in the terminal within a given viewport. (prints ~ for empty lines)
printLines :: [String] -> Viewport -> Int -> IO ()
printLines lines' (Viewport rows' columns' initialRow' initialColumn') row = do
  hideCursor
  handleLines
  showCursor
  where
    viewport' = Viewport rows' columns' initialRow' initialColumn'
    handleLines
      | row == (rows' - 1) 
        = do 
          putStr ""
      | null lines' 
        = do
          putStrLn "~" 
          printLines lines' viewport' (row + 1)
      | otherwise 
        = do 
          moveCursor (Cursor 0 row)
          putStrLn (head lines')
          printLines (tail lines') viewport' (row + 1)