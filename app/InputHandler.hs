module InputHandler where

import Editor.Cursor
import Editor.EditorState
import Editor.ExtendedPieceTable
import Editor.ModeManager

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
  | inputChar `elem` ["h", "j", "k", "l"] && mode state == Normal =
      do
        let direction = head inputChar
         in handleMovement state direction False
  | not special || inputChar `elem` ["\ESC", "\n"] = handleMode state inputChar
  | otherwise = return state
  where
    special =
      case inputChar of
        (char : _) -> fromEnum char < 32
        [] -> False

-- Handles key press associated with movement
handleMovement :: EditorState -> Char -> Bool -> IO EditorState
handleMovement state direction isArrow
  | isArrow && (mode state == Insert || mode state == Replace) =
      do
        let (pieces, originalBuffer, addBuffer, insertBuffer, _, lineSizes) = insertText (extendedPieceTable state)
            newCursor = updateCursor direction (cursor state) lineSizes True
            newInsertStartIndex = cursorXYToStringIndex newCursor lineSizes 0 0
            newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, lineSizes)
         in return state {cursor = newCursor, extendedPieceTable = newExtendedPieceTable}
  | mode state == Insert || mode state == Replace = handleInsertMode state [direction]
  | mode state == Normal || mode state == Visual =
      do
        let (_, _, _, _, _, lineSizes) = extendedPieceTable state
            newCursor = updateCursor direction (cursor state) lineSizes False
         in return state {cursor = newCursor}
  | otherwise = return state
