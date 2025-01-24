module Editor.InsertMode where

import System.IO
import System.Console.ANSI

import Editor.Cursor
import Editor.EditorState


handleInsertMode :: EditorState -> [Char] -> EditorState
handleInsertMode state "\27" = 
  let newMode = Normal
  in state { mode = newMode }
-- Caso de inserção normal de caracteres
handleInsertMode state inputChar = 
  let (pieces, ogBuffer, addBuffer, insertBuffer, sizesSeq) = (pieceTable state)
      (iBuffer, startPos) = insertBuffer
      newInsertBuffer = iBuffer ++ inputChar
      newStartPos = (cursorToPieceTablePos (cursor state) sizesSeq 0 0)
  in (state { pieceTable = (pieces, ogBuffer, addBuffer, (newInsertBuffer, newStartPos), sizesSeq)})

-- Creates a function to transform a single string buffer into a 
-- array of strings, where each string represents a line in the grid
splitLines :: String -> [String]
splitLines buffer = 
  let (line, rest) = break (== '\n') buffer
  in line : if null rest then [] else splitLines (tail rest)


insertCommandManager :: String -> Cursor -> Char -> (Cursor, String)
insertCommandManager buffer cursor char = (cursor, buffer)
  -- printBufferToGrid buffer
