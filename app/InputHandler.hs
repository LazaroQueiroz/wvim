{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module InputHandler where

import Editor.Cursor
import Editor.EditorState
import Editor.ExtendedPieceTable
import Editor.ModeManager
import Editor.Viewport

-- Handles key press events and updates the EditorState accordingly based on the mode (Insert, Normal, or Command).
handleKeyPress :: EditorState -> [Char] -> IO EditorState
handleKeyPress state inputChar
  | inputChar `elem` ["\ESC[A", "\ESC[B", "\ESC[C", "\ESC[D"] =
      do
        let direction = case inputChar of
              "\ESC[A" -> 'k'
              "\ESC[B" -> 'j'
              "\ESC[C" -> 'l'
              "\ESC[D" -> 'h'
              _ -> head inputChar
         in handleMovement state direction True
  | inputChar `elem` ["h", "j", "k", "l"] =
      do
        let direction = head inputChar
         in handleMovement state direction False
  | not special || inputChar `elem` ["\ESC", "\ESC[2~", "\ESC[3~", "\n", "\DC2"] = handleMode state inputChar
  | otherwise = return state
  where
    special =
      case inputChar of
        (char : _) -> fromEnum char < 32
        [] -> False

-- Função auxiliar para lidar com o movimento do cursor
handleMovement :: EditorState -> Char -> Bool -> IO EditorState
handleMovement state direction isArrow
  | isArrow && (mode state == Insert || mode state == Replace) =
      do
        let (pieces, originalBuffer, addBuffer, insertBuffer, _, lineSizes) = insertText (extendedPieceTable state)
            newViewport = updateViewport viewport' (x (cursor state), y (cursor state)) lineSizes direction True
            newCursor = updateCursor direction (cursor state) newViewport lineSizes True
            newInsertStartIndex = cursorXYToStringIndex newCursor (initialRow newViewport) (initialColumn newViewport) lineSizes 0 (negate (initialRow newViewport))
            newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, lineSizes)
         in return state {cursor = newCursor, extendedPieceTable = newExtendedPieceTable, viewport = newViewport}
  | mode state == Insert || mode state == Replace = handleInsertMode state [direction]
  | mode state == Normal || mode state == Visual =
      do
        let (_, _, _, _, _, lineSizes) = extendedPieceTable state
            newViewport = updateViewport viewport' (x (cursor state), y (cursor state)) lineSizes direction False
            newCursor = updateCursor direction (cursor state) newViewport lineSizes False
         in return state {cursor = newCursor, viewport = newViewport}
  | otherwise = return state
  where
    (EditorState _ _ _ viewport' _ _ _ _) = state
