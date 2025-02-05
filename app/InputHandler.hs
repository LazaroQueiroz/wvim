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
    | inputChar `elem` ["\ESC[A", "\ESC[B", "\ESC[C", "\ESC[D"]
      = do
        let direction = case inputChar of
              "\ESC[A" -> 'k'
              "\ESC[B" -> 'j'
              "\ESC[C" -> 'l'
              "\ESC[D" -> 'h'
         in handleMovement state direction 
    | not special || inputChar `elem` ["\ESC", "\ESC[2~", "\ESC[3~", "\n","\DC2"] 
      = do
        case mode state of
          Normal -> handleNormalMode state inputChar
          Visual -> handleNormalMode state inputChar --Em breve terá a propria classe
          Insert -> handleInsertMode state inputChar
          Replace -> handleInsertMode state inputChar --Em breve terá a propria classe
          Command -> handleCommandMode state inputChar
    | otherwise = return state
    where
      special = 
        case inputChar of
          (char:_) -> fromEnum char < 32 
          []    -> False

-- Função auxiliar para Insert e Replace
handleMovement :: EditorState -> Char -> IO EditorState
handleMovement state direction
  | mode state == Insert || mode state == Replace 
    = do
    let (pieces, originalBuffer, addBuffer, insertBuffer, _, lineSizes) = insertText (extendedPieceTable state)
        newCursor = updateCursor direction (cursor state) lineSizes True
        newInsertStartIndex = cursorXYToStringIndex newCursor lineSizes 0 0
        newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, lineSizes)
     in return state {cursor = newCursor, extendedPieceTable = newExtendedPieceTable}
  | mode state == Normal || mode state == Visual
    = do
    let (_, _, _, _, _, lineSizes) = extendedPieceTable state 
        newCursor = updateCursor direction (cursor state) lineSizes False
     in return state {cursor = newCursor}
  | otherwise = return state