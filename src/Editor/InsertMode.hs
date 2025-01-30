module Editor.InsertMode where

import Editor.Cursor (Cursor (Cursor), updateCursorPosition)
import Editor.EditorState
import Editor.ExtendedPieceTable (
  deleteText,
  insertText,
  updateLinesSizes,
 )
import Utils

handleInsertMode :: EditorState -> [Char] -> IO EditorState
handleInsertMode currentState "\ESC" = do
  let newMode = Normal
      extPieceTable = extendedPieceTable currentState
      newExtendedPieceTable = insertText extPieceTable
  return currentState{mode = newMode, extendedPieceTable = newExtendedPieceTable}
handleInsertMode currentState "\DEL" = do
  let extPieceTable = extendedPieceTable currentState
      Cursor x' y' = cursor currentState
      (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = extPieceTable
      newLinesSizes' = updateLinesSizes "\DEL" (cursor currentState) linesSizes
      newExtendedPieceTable
        | not (null insertBuffer) =
            let newInsertBuffer = take (length insertBuffer - 1) insertBuffer
             in (pieces, originalBuffer, addBuffer, newInsertBuffer, insertStartIndex, newLinesSizes')
        | (x' == 0) && (y' == 0) = extPieceTable
        | otherwise =
            let (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', _) = deleteText insertStartIndex 1 extPieceTable
             in (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', newLinesSizes')
  return currentState{extendedPieceTable = newExtendedPieceTable, cursor = updateCursorPosition (cursor currentState) "\DEL" (nth x' linesSizes), fileStatus = NotSaved}

-- Caso de inserção normal de caracteres
handleInsertMode currentState inputChar = do
  let (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = extendedPieceTable currentState
      newInsertBuffer = insertBuffer ++ inputChar
      newLinesSizes = updateLinesSizes inputChar (cursor currentState) linesSizes
  return currentState{extendedPieceTable = (pieces, originalBuffer, addBuffer, newInsertBuffer, insertStartIndex, newLinesSizes), cursor = updateCursorPosition (cursor currentState) inputChar 0, fileStatus = NotSaved}
