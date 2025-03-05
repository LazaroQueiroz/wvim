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

updateEditorStateViewport :: EditorState -> EditorState
updateEditorStateViewport currentState =
  let currentViewport = viewport currentState 
      (_, _, _, _, _, linesSizes') = extendedPieceTable currentState
      cursor' = cursor currentState
      y' = y cursor'
      x' = x cursor'
      mode' = mode currentState
      partiallyFixedViewport = updateViewport currentViewport (x', y') linesSizes' (mode' == Insert)
      newViewport = updateViewport partiallyFixedViewport (x', y') linesSizes' (mode' == Insert)
  in currentState {viewport = newViewport}

-- Função auxiliar para lidar com o movimento do cursor
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
  where
    (EditorState _ _ _ viewport' _ _ _ _ _ _ _ _) = state
