module Editor.NormalMode where

import Editor.Cursor (updateCursor)
import Editor.EditorState
  ( EditorState (cursor, extendedPieceTable, mode),
    Mode (Command, Insert, Replace, Visual),
    updateEditorStateCursor,
  )
import Editor.ExtendedPieceTable
import System.Console.ANSI ()
import System.IO ()

-- Handles user input in Normal mode, updating the editor state accordingly and switching to Insert mode or Command mode.
handleNormalMode :: EditorState -> [Char] -> IO EditorState
handleNormalMode currentState inputChar
  | inputChar `elem` ["i", "I", "\ESC[2~"] = switchToInsertMode currentState False
  | inputChar `elem` ["a", "A"] = switchToInsertMode currentState True
  | inputChar `elem` ["r", "R"] = return currentState {mode = Replace} -- Switch to Replace Mode
  | inputChar `elem` ["v", "V"] = return currentState {mode = Visual} -- Switch to Visual Mode
  | inputChar `elem` ["u", "U"] = return currentState -- TODO: UNDO
  | inputChar == "\DC2" = return currentState -- TODO: REDO
  | inputChar == "\DEL" = return (updateEditorStateCursor currentState "h")
  | inputChar == ":" = return currentState {mode = Command} -- Switch to Command mode.
  | otherwise = return (updateEditorStateCursor currentState inputChar)

-- Switches to Insert mode ("i" or "I"), optionally moving the cursor forward ("a" or "A").
switchToInsertMode :: EditorState -> Bool -> IO EditorState
switchToInsertMode currentState moveCursor = do
  let newMode = Insert
      (pieces, originalBuffer, addBuffer, insertBuffer, _, linesSizes) = extendedPieceTable currentState
      newCursor
        | moveCursor = updateCursor 'l' (cursor currentState) linesSizes True
        | otherwise = cursor currentState
      newInsertStartIndex = cursorXYToStringIndex newCursor linesSizes 0 0
      newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, linesSizes)
   in return currentState {mode = newMode, extendedPieceTable = newExtendedPieceTable, cursor = newCursor}
