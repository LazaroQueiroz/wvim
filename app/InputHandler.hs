-- TODO "Closed" fora do pattern matching
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module InputHandler where

import Editor.CommandMode (handleCommandMode)
import Editor.Cursor
import Editor.EditorState
import Editor.InsertMode
import Editor.NormalMode

handleKeyPress :: EditorState -> [Char] -> IO EditorState
handleKeyPress state "\ESC[A" = do
  let (_, _, _, _, _, lineSizes) = extendedPieceTable state
  return state {cursor = updateCursor 'k' (cursor state) lineSizes}
handleKeyPress state "\ESC[B" = do
  let (_, _, _, _, _, lineSizes) = extendedPieceTable state
  return state {cursor = updateCursor 'j' (cursor state) lineSizes}
handleKeyPress state "\ESC[C" = do
  let (_, _, _, _, _, lineSizes) = extendedPieceTable state
  return state {cursor = updateCursor 'l' (cursor state) lineSizes}
handleKeyPress state "\ESC[D" = do
  let (_, _, _, _, _, lineSizes) = extendedPieceTable state
  return state {cursor = updateCursor 'h' (cursor state) lineSizes}
handleKeyPress state inputChar = case mode state of
  Normal -> do handleNormalMode state inputChar
  Insert -> do handleInsertMode state inputChar
  Command -> do handleCommandMode state inputChar
