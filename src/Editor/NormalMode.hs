module Editor.NormalMode where

import Editor.Cursor ()
import Editor.EditorState
  ( EditorState (cursor, extendedPieceTable, mode),
    Mode (Command, Insert),
    updateEditorStateCursor,
  )
import Editor.ExtendedPieceTable
import System.Console.ANSI ()
import System.IO ()

handleNormalMode :: EditorState -> [Char] -> IO EditorState
handleNormalMode currentState "i" = do
  let newMode = Insert
      (pieces, originalBuffer, addBuffer, insertBuffer, _, linesSizes) = extendedPieceTable currentState
      newInsertStartIndex = cursorXYToStringIndex (cursor currentState) linesSizes 0 0
      newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, linesSizes)
  return currentState {mode = newMode, extendedPieceTable = newExtendedPieceTable}
handleNormalMode currentState "I" = do
  let newMode = Insert
      (pieces, originalBuffer, addBuffer, insertBuffer, _, linesSizes) = extendedPieceTable currentState
      newInsertStartIndex = cursorXYToStringIndex (cursor currentState) linesSizes 0 0
      newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, linesSizes)
  return currentState {mode = newMode, extendedPieceTable = newExtendedPieceTable}
handleNormalMode currentState ":" = do
  let newMode = Command
  return currentState {mode = newMode}
handleNormalMode currentState inputChar = do
  return (updateEditorStateCursor currentState inputChar)
