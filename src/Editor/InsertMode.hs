module Editor.InsertMode where

import Editor.Cursor (Cursor (Cursor), updateCursor, updateCursorPosition)
import Editor.EditorState
import Editor.ExtendedPieceTable (
  deleteText,
  insertText,
  updateLinesSizes,
 )
import Utils

handleInsertMode :: EditorState -> [Char] -> IO EditorState
-- Caso de sair do modo
handleInsertMode currentState "\ESC" = do
  let newMode = Normal
      extPieceTable = extendedPieceTable currentState
      newExtendedPieceTable = insertText extPieceTable
      (_, _, _, _, _, linesSizes') = newExtendedPieceTable
      newCursor = updateCursor 'h' (cursor currentState) linesSizes' False
  return currentState{mode = newMode, extendedPieceTable = newExtendedPieceTable, cursor = newCursor}
-- Caso de deletar caracteres
handleInsertMode currentState "\DEL"
  | x' == 0 && y' == 0 = return currentState 
  | otherwise = do
      let extPieceTable = extendedPieceTable currentState
          (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = extPieceTable
          newLinesSizes = updateLinesSizes "\DEL" (cursor currentState) linesSizes
          newExtendedPieceTable
            | not (null insertBuffer) =
                let newInsertBuffer = take (length insertBuffer - 1) insertBuffer
                 in (pieces, originalBuffer, addBuffer, newInsertBuffer, insertStartIndex, newLinesSizes)
            | otherwise =
                let (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', _) = deleteText insertStartIndex 1 extPieceTable
                 in (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', newLinesSizes)
      return currentState{extendedPieceTable = newExtendedPieceTable, cursor = updateCursorPosition (cursor currentState) "\DEL" (nth x' linesSizes), fileStatus = NotSaved}
  where Cursor x' y' = cursor currentState
-- Caso de inserir caracteres
handleInsertMode currentState inputChar = do
  let (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = extendedPieceTable currentState
      newInsertBuffer = if inputChar == "\DEL" then "" else insertBuffer ++ inputChar
      newLinesSizes = updateLinesSizes inputChar (cursor currentState) linesSizes
  return currentState{extendedPieceTable = (pieces, originalBuffer, addBuffer, newInsertBuffer, insertStartIndex, newLinesSizes), cursor = updateCursorPosition (cursor currentState) inputChar 0, fileStatus = NotSaved}
