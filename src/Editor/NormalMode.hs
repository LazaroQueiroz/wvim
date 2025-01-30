module Editor.NormalMode where

import System.IO
import System.Console.ANSI
import Editor.EditorState
import Editor.Cursor
import Editor.ExtendedPieceTable

handleNormalMode :: EditorState -> [Char] -> IO EditorState 
handleNormalMode currentState "i" = do
  let newMode = Insert
      (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = (extendedPieceTable currentState)
      newInsertStartIndex = (cursorXYToStringIndex (cursor currentState) linesSizes 0 0)
      newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, linesSizes)
  return currentState { mode = newMode, extendedPieceTable = newExtendedPieceTable }

handleNormalMode currentState "I" = do
  let newMode = Insert
      (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = (extendedPieceTable currentState)
      newInsertStartIndex = (cursorXYToStringIndex (cursor currentState) linesSizes 0 0)
      newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, linesSizes)
  return currentState { mode = newMode, extendedPieceTable = newExtendedPieceTable }

handleNormalMode currentState ":" = do
  let newMode = Command
  return currentState { mode = newMode }

handleNormalMode currentState inputChar = do
  let nextState = updateEditorStateCursor currentState inputChar
  return nextState



