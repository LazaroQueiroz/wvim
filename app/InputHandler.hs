-- TODO "Closed" fora do pattern matching
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module InputHandler where

import Editor.CommandMode (handleCommandMode)
import Editor.Cursor
import Editor.EditorState
import Editor.InsertMode
import Editor.NormalMode

handleKeyPress :: EditorState -> [Char] -> IO EditorState
handleKeyPress state key = do
  let (_, _, _, _, _, lineSizes) = extendedPieceTable state
      isInsertMode = mode state == Insert
  case key of
    "\ESC[A" -> return state{cursor = updateCursor 'k' (cursor state) lineSizes isInsertMode}
    "\ESC[B" -> return state{cursor = updateCursor 'j' (cursor state) lineSizes isInsertMode}
    "\ESC[C" -> return state{cursor = updateCursor 'l' (cursor state) lineSizes isInsertMode}
    "\ESC[D" -> return state{cursor = updateCursor 'h' (cursor state) lineSizes isInsertMode}
    _ -> case mode state of
      Normal -> handleNormalMode state key
      Insert -> handleInsertMode state key
      Command -> handleCommandMode state key
