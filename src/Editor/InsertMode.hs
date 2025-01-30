module Editor.InsertMode where

import Editor.Cursor (Cursor (Cursor), updateCursorPosition)
import Editor.EditorState
import Editor.ExtendedPieceTable
  ( deleteText,
    insertText,
    updateLinesSizes,
  )
import Utils

handleInsertMode :: EditorState -> [Char] -> IO EditorState
handleInsertMode currentState "\ESC" = do
  let newMode = Normal
      extPieceTable = extendedPieceTable currentState
      newExtendedPieceTable = Editor.ExtendedPieceTable.insertText extPieceTable
  return currentState {mode = newMode, extendedPieceTable = newExtendedPieceTable}
handleInsertMode currentState "\DEL" = do
  let extPieceTable = extendedPieceTable currentState
      Editor.Cursor.Cursor cx cy = cursor currentState
      (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = extPieceTable
      newLinesSizes = Editor.ExtendedPieceTable.updateLinesSizes "\DEL" (cursor currentState) linesSizes
      newExtendedPieceTable
        | not (null insertBuffer) =
            let newInsertBuffer = take (length insertBuffer - 1) insertBuffer
             in (pieces, originalBuffer, addBuffer, newInsertBuffer, insertStartIndex, newLinesSizes)
        | (cx == 0) && (cy == 0) = extPieceTable
        | otherwise =
            let (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', newLinesSizes') = Editor.ExtendedPieceTable.deleteText insertStartIndex 1 extPieceTable
             in (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', newLinesSizes')
  return currentState {extendedPieceTable = newExtendedPieceTable, cursor = Editor.Cursor.updateCursorPosition (cursor currentState) "\DEL" (nth cx linesSizes), fileStatus = NotSaved}

-- Caso de inserção normal de caracteres
handleInsertMode currentState inputChar = do
  let (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = extendedPieceTable currentState
      newInsertBuffer = insertBuffer ++ inputChar
      newLinesSizes = Editor.ExtendedPieceTable.updateLinesSizes inputChar (cursor currentState) linesSizes
  return currentState {extendedPieceTable = (pieces, originalBuffer, addBuffer, newInsertBuffer, insertStartIndex, newLinesSizes), cursor = Editor.Cursor.updateCursorPosition (cursor currentState) inputChar 0, fileStatus = NotSaved}
