{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module InputHandler where

import Editor.CommandMode (handleCommandMode)
import Editor.Cursor
import Editor.EditorState
import Editor.ExtendedPieceTable
import Editor.InsertMode
import Editor.NormalMode

handleKeyPress :: EditorState -> [Char] -> IO EditorState
handleKeyPress state inputChar
  | mode state == Insert && inputChar `elem` ["\ESC[A", "\ESC[B", "\ESC[C", "\ESC[D"] = do
      let direction = case inputChar of
            "\ESC[A" -> 'k'
            "\ESC[B" -> 'j'
            "\ESC[C" -> 'l'
            "\ESC[D" -> 'h'
          (pieces, originalBuffer, addBuffer, insertBuffer, _, lineSizes) = insertText (extendedPieceTable state)
          newCursor = updateCursor direction (cursor state) lineSizes True
          newInsertStartIndex = cursorXYToStringIndex newCursor lineSizes 0 0
          newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, lineSizes)
      return state{cursor = newCursor, extendedPieceTable = newExtendedPieceTable}
  | mode state == Normal && inputChar `elem` ["\ESC[A", "\ESC[B", "\ESC[C", "\ESC[D"] = do
      let direction = case inputChar of
            "\ESC[A" -> 'k'
            "\ESC[B" -> 'j'
            "\ESC[C" -> 'l'
            "\ESC[D" -> 'h'
          (_, _, _, _, _, lineSizes) = extendedPieceTable state
      return state{cursor = updateCursor direction (cursor state) lineSizes False}
  | otherwise = case mode state of
      Normal -> handleNormalMode state inputChar
      Insert -> handleInsertMode state inputChar
      Command -> handleCommandMode state inputChar
