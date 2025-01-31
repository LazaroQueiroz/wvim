module Editor.NormalMode where

import Editor.Cursor (updateCursor)
import Editor.EditorState
  ( EditorState (cursor, extendedPieceTable, mode),
    Mode (Command, Insert),
    updateEditorStateCursor,
  )
import Editor.ExtendedPieceTable
import System.Console.ANSI ()
import System.IO ()

handleNormalMode :: EditorState -> [Char] -> IO EditorState
handleNormalMode currentState inputChar
  | inputChar `elem` ["i", "I"] =
      let newMode = Insert
          (pieces, originalBuffer, addBuffer, insertBuffer, _, linesSizes) = extendedPieceTable currentState
          newInsertStartIndex = cursorXYToStringIndex (cursor currentState) linesSizes 0 0
          newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, linesSizes)
      in return currentState {mode = newMode, extendedPieceTable = newExtendedPieceTable}
  | inputChar `elem` ["a", "A"] =
      let newMode = Insert
          (pieces, originalBuffer, addBuffer, insertBuffer, _, linesSizes) = extendedPieceTable currentState
          newCursor = updateCursor 'l' (cursor currentState) linesSizes True
          newInsertStartIndex = cursorXYToStringIndex newCursor linesSizes 0 0
          newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, linesSizes)
      in return currentState {mode = newMode, extendedPieceTable = newExtendedPieceTable, cursor = newCursor}
  | inputChar == ":" = 
      return currentState {mode = Command}
  | otherwise =
      return (updateEditorStateCursor currentState inputChar)