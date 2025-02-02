module Editor.InsertMode where

import Editor.Cursor (Cursor (Cursor), updateCursor, updateCursorPosition)
import Editor.EditorState
import Editor.ExtendedPieceTable (
  deleteText,
  insertText,
  updateLinesSizes,
 )
import Utils

-- Handles user input in Insert mode, updating the editor state accordingly.
handleInsertMode :: EditorState -> [Char] -> IO EditorState
handleInsertMode currentState inputChar
  | inputChar == "\ESC" = handleEscape currentState  -- Switches to Normal mode
  | inputChar == "\DEL" && x' == 0 && y' == 0 = return currentState -- Don't delete character 
  | inputChar == "\DEL" = handleDelete currentState -- Delete character
  | otherwise = handleInsert currentState inputChar -- Inserts character
  where
    Cursor x' y' = cursor currentState
  
-- Handles exiting Insert mode
handleEscape :: EditorState -> IO EditorState
handleEscape currentState = do
  let newMode = Normal
      extPieceTable = extendedPieceTable currentState
      newExtendedPieceTable = insertText extPieceTable
      (_, _, _, _, _, linesSizes') = newExtendedPieceTable
      newCursor = updateCursor 'h' (cursor currentState) linesSizes' False
   in return currentState { mode = newMode, extendedPieceTable = newExtendedPieceTable, cursor = newCursor }

-- Handles character insertion
handleInsert :: EditorState -> [Char] -> IO EditorState
handleInsert currentState inputChar = do
  let extPieceTable = extendedPieceTable currentState
      (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = extPieceTable
      newInsertBuffer = insertBuffer ++ inputChar
      newLinesSizes = updateLinesSizes inputChar (cursor currentState) linesSizes
      newCursor = updateCursorPosition (cursor currentState) inputChar 0
      newExtendedPieceTable = (pieces, originalBuffer, addBuffer, newInsertBuffer, insertStartIndex, newLinesSizes)
   in return currentState { extendedPieceTable = newExtendedPieceTable, cursor = newCursor, fileStatus = NotSaved }

-- Handles character deletion
handleDelete :: EditorState -> IO EditorState 
handleDelete currentState = do
  let extPieceTable = extendedPieceTable currentState
      (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = extPieceTable
      newLinesSizes = updateLinesSizes "\DEL" (cursor currentState) linesSizes
      Cursor x' _ = cursor currentState
      newCursor = updateCursorPosition (cursor currentState) "\DEL" (nth x' linesSizes)
      newExtendedPieceTable
        | not (null insertBuffer) =
            let newInsertBuffer = take (length insertBuffer - 1) insertBuffer
             in (pieces, originalBuffer, addBuffer, newInsertBuffer, insertStartIndex, newLinesSizes)
        | otherwise =
            let (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', _) = deleteText insertStartIndex 1 extPieceTable
             in (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', newLinesSizes)
   in return currentState { extendedPieceTable = newExtendedPieceTable, cursor = newCursor, fileStatus = NotSaved }