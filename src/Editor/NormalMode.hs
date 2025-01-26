module Editor.NormalMode where

import System.IO
import System.Console.ANSI
import Editor.EditorState
import Editor.Cursor
import Editor.ExtendedPieceTable

handleNormalMode :: EditorState -> [Char] -> EditorState 
handleNormalMode currentState "i" = 
  let newMode = Insert
      (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = (extendedPieceTable currentState)
      newInsertStartIndex = (cursorXYToStringIndex (cursor currentState) linesSizes 0 0)
      newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, linesSizes)
  in currentState { mode = newMode, extendedPieceTable = newExtendedPieceTable }
handleNormalMode currentState "I" = 
  let newMode = Insert
      (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = (extendedPieceTable currentState)
      newInsertStartIndex = (cursorXYToStringIndex (cursor currentState) linesSizes 0 0)
      newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, linesSizes)
  in currentState { mode = newMode, extendedPieceTable = newExtendedPieceTable }
handleNormalMode currentState ":" = 
  let newMode = Command
  in currentState { mode = newMode }
handleNormalMode currentState inputChar =
  updateEditorStateCursor currentState inputChar



