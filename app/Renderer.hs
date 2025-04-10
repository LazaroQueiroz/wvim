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
import Render

-- Renders the current state of the editor: the Viewport (actual content of the file), the status bar (which contains essential information about the editor state and the file) and renders the correct state of the cursor (position and style).
-- @param editorState :: EditorState -> current state of the editor.
renderState :: EditorState -> IO ()
renderState (EditorState mode' extendedPieceTable' cursor' viewport' _ filename' statusBar' commandBuffer' undoStack' redoStack' visualModeStartIndex' copyBuffer' searchBuffer') = do
  clearScreen
  renderViewport extendedPieceTable' cursor' viewport' filename'
  renderStatusBar mode' viewport' cursor' filename' (statusMode statusBar') (errorMessage statusBar') commandBuffer' extendedPieceTable' undoStack' redoStack' copyBuffer' searchBuffer'
  renderCursor mode' cursor' viewport'

-- Renders the viewport, meaning that it renders all the contents of the files given the current dimensions of the viewport.
renderViewport :: ExtendedPieceTable -> Cursor -> Viewport -> String -> IO ()
renderViewport extendedPieceTable' _ viewport' _ = do
  let lines' = extendedPieceTableToLineArray extendedPieceTable'
  printLines lines' viewport' 0
  hFlush stdout

-- Renders the status bar with mode, cursor position, file info, and errors.
renderStatusBar :: Mode -> Viewport -> Cursor -> String -> StatusMode -> String -> String -> ExtendedPieceTable -> [EditorState] -> [EditorState] -> String -> String -> IO ()
renderStatusBar mode' viewport' cursor' filename' sBarMode errorMsg commandBuffer' extendedPieceTable' undoStack' redoStack' copyBuffer' searchBuffer' = do
  moveCursor (Cursor 0 (rows viewport'))
  putStr $ "| " ++ showMode ++ " | "
  putStr $ showPath ++ " | "
  case mode' of
    Command -> do
      putStr $ ":" ++ commandBuffer'
    Substitution -> do
      putStr $ show (x cursor' + 1) ++ ", " ++ show (y cursor' + 1) ++ " | "
      putStr $ searchBuffer' ++ " | "
    _ -> do
      putStr $ show (x cursor' + 1) ++ ", " ++ show (y cursor' + 1) ++ " | "
      putStr $ getLineProgress extendedPieceTable' cursor' (initialRow viewport') ++ " | "
  where
    (Viewport rows' columns' initialRow' initialColumn') = viewport' -- Debug purpose
    (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = extendedPieceTable' -- Debug purpose

    showFileName
      | null filename' = "None"
      | otherwise = filename'

    showMode =
      case mode' of
        Normal -> "Normal"
        Insert -> "Insert"
        Command -> "Command"
        Replace -> "Replace"
        Visual -> "Visual"
        Substitution -> "Substitution"
        _ -> "None"

    showPath =
      case sBarMode of
        NoException -> "Path: " ++ showFileName
        Exception -> errorMsg

-- Returns the cursor's vertical position as "Top", "Bot", or a percentage.
getLineProgress :: ExtendedPieceTable -> Cursor -> Int -> String
getLineProgress (_, _, _, _, _, linesSizes) cursor' _
  | x cursor' == 0 = "Top"
  | x cursor' == length linesSizes - 1 = "Bot"
  | otherwise = show ((x cursor' + 1) * 100 `div` length linesSizes) ++ "%"

-- Renders the cursor in the terminal based on its position and style.
renderCursor :: Mode -> Cursor -> Viewport -> IO ()
renderCursor curMode (Cursor x' y') viewport' = do
  setCursorPosition (min (x' - initialRow viewport') (rows viewport' - 2)) (min (y' - initialColumn viewport') (columns viewport' - 1))
  case curMode of
    Normal -> putStr "\ESC[1 q"
    Visual -> putStr "\ESC[1 q"
    Replace -> putStr "\ESC[1 q"
    Insert -> putStr "\ESC[5 q"
    Command -> putStr "\ESC[5 q"
    Substitution -> putStr "\ESC[5 q"
    _ -> putStr "\ESC[5 q"
  hFlush stdout

-- Renders lines in the terminal within a given viewport. (prints ~ for empty lines)
printLines :: [String] -> Viewport -> Int -> IO ()
printLines lines' (Viewport rows' columns' initialRow' initialColumn') row = do
  let visibleLines = drop initialRow' lines'
  printLinesLoop visibleLines (Viewport rows' columns' initialRow' initialColumn') row

printLinesLoop :: [String] -> Viewport -> Int -> IO ()
printLinesLoop visibleLines (Viewport rows' columns' initialRow' initialColumn') row = do
  hideCursor
  handleLines
  showCursor
  where
    viewport' = Viewport rows' columns' initialRow' initialColumn'
    handleLines
      | row >= (rows' - 1) =
          do
            putStr ""
      | null visibleLines =
          do
            putStrLn "~"
            printLinesLoop visibleLines viewport' (row + 1)
      | otherwise =
          do
            moveCursor (Cursor 0 row)
            putStrLn (take (columns' - 1) (drop initialColumn' (head visibleLines)))
            printLinesLoop (tail visibleLines) viewport' (row + 1)
