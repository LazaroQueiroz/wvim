{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module InputHandler where

import Editor.CommandMode (handleCommandMode)
import Editor.Cursor
import Editor.EditorState
import Editor.ExtendedPieceTable
import Editor.InsertMode
import Editor.NormalMode

-- Handles key press events and updates the EditorState accordingly based on the mode (Insert, Normal, or Command).
handleKeyPress :: EditorState -> [Char] -> IO EditorState
handleKeyPress state inputChar
    | inputChar `elem` ["\ESC[A", "\ESC[B", "\ESC[C", "\ESC[D"] =
      let direction = case inputChar of
            "\ESC[A" -> 'k'
            "\ESC[B" -> 'j'
            "\ESC[C" -> 'l'
            "\ESC[D" -> 'h'
       in case mode state of
          Insert ->
            let (pieces, originalBuffer, addBuffer, insertBuffer, _, lineSizes) = insertText (extendedPieceTable state)
                newCursor = updateCursor direction (cursor state) lineSizes True
                newInsertStartIndex = cursorXYToStringIndex newCursor lineSizes 0 0
                newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, lineSizes)
            in return state {cursor = newCursor, extendedPieceTable = newExtendedPieceTable}
          Normal ->
            let (_, _, _, _, _, lineSizes) = extendedPieceTable state
                newCursor = updateCursor direction (cursor state) lineSizes False
            in return state {cursor = newCursor}
          Command -> return state --TODO: Movement on commandText
    | otherwise 
      = 
        case mode state of
          Normal -> handleNormalMode state inputChar
          Insert -> handleInsertMode state inputChar
          Command -> handleCommandMode state inputChar