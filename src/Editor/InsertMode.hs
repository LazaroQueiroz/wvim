module Editor.InsertMode where

import System.IO
import System.Console.ANSI

import Editor.Cursor
import Editor.EditorState
import Editor.ExtendedPieceTable

import Utils

handleInsertMode :: EditorState -> [Char] -> EditorState
handleInsertMode currentState "\ESC" = 
  let newMode = Normal
      extPieceTable = (extendedPieceTable currentState)
      (Cursor x y) = (cursor currentState)
      (_, _, _, insertBuffer, insertStartIndex, linesSizes) = extPieceTable
      -- insertStartIndex = (cursorXYToStringIndex (Cursor x y) linesSizes 0 0) - (length insertBuffer)
      newExtendedPieceTable = (insertText extPieceTable)
  in (currentState { mode = newMode, extendedPieceTable = newExtendedPieceTable})

handleInsertMode currentState "\DEL" = 
  let extPieceTable = (extendedPieceTable currentState)
      (Cursor x y) = (cursor currentState)
      (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = extPieceTable
      -- insertStartIndex = (cursorXYToStringIndex (Cursor x y) linesSizes 0 0) - (length insertBuffer)
      newLinesSizes = (updateLinesSizes "\DEL" (cursor currentState) linesSizes)
      newExtendedPieceTable = 
        if (length insertBuffer) > 0 then 
          let newInsertBuffer = (take ((length insertBuffer) - 1) insertBuffer)
          in (pieces, originalBuffer, addBuffer, newInsertBuffer, insertStartIndex, newLinesSizes)
        else 
          let (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = (deleteText (insertStartIndex) 1 extPieceTable)
          in (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, newLinesSizes)
  in (currentState { extendedPieceTable = newExtendedPieceTable, cursor = (updateCursorPosition (cursor currentState) "\DEL" (nth x linesSizes))})

-- Caso de inserção normal de caracteres
handleInsertMode currentState inputChar = 
  let (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = (extendedPieceTable currentState)
      newInsertBuffer = insertBuffer ++ inputChar
      -- newStartPos = (cursorToPieceTablePos (Cursor x y) sizesSeq 0 0) - (length insertBuffer)
      newLinesSizes = (updateLinesSizes inputChar (cursor currentState) linesSizes)
  in (currentState { extendedPieceTable = (pieces, originalBuffer, addBuffer, newInsertBuffer, insertStartIndex, newLinesSizes), cursor = (updateCursorPosition (cursor currentState) inputChar 0)})

-- -- Creates a function to transform a single string buffer into a 
-- -- array of strings, where each string represents a line in the grid
-- splitLines :: String -> [String]
-- splitLines buffer = 
--   let (line, rest) = break (== '\n') buffer
--   in line : if null rest then [] else splitLines (tail rest)

