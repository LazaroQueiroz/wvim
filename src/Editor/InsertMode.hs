module Editor.InsertMode where

import System.IO
import System.Console.ANSI

import Editor.Cursor
import Editor.EditorState
import Editor.PieceTable

import Utils

handleInsertMode :: EditorState -> [Char] -> EditorState
handleInsertMode state "\ESC" = 
  let newMode = Normal
      pTable = (pieceTable state)
      (Cursor x y) = (cursor state)
      (_, _, _, (iBuffer, startPos), sizesSeq) = pTable
      pieceTablePos = (cursorToPieceTablePos (Cursor x y) sizesSeq 0 0) - (length iBuffer)
      newPieceTable = (insertText iBuffer pieceTablePos pTable)
  in (state { mode = newMode, pieceTable = newPieceTable})

-- Caso de inserção normal de caracteres
handleInsertMode state inputChar = 
  let (pieces, ogBuffer, addBuffer, insertBuffer, sizesSeq) = (pieceTable state)
      (iBuffer, startPos) = insertBuffer
      newInsertBuffer = iBuffer ++ inputChar
      (Cursor x y) = (cursor state)
      newStartPos = (cursorToPieceTablePos (Cursor x y) sizesSeq 0 0) - (length iBuffer)
      newSizesSeq = (updateSizesSeq inputChar (Cursor x y) sizesSeq)
  in (state { pieceTable = (pieces, ogBuffer, addBuffer, (newInsertBuffer, newStartPos), newSizesSeq), cursor = (newCursorPositionFromChar (cursor state) inputChar)})

-- -- Creates a function to transform a single string buffer into a 
-- -- array of strings, where each string represents a line in the grid
-- splitLines :: String -> [String]
-- splitLines buffer = 
--   let (line, rest) = break (== '\n') buffer
--   in line : if null rest then [] else splitLines (tail rest)

