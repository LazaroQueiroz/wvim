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

-- Handles user input in Normal mode, updating the editor state accordingly and switching to Insert mode or Command mode.
handleNormalMode :: EditorState -> [Char] -> IO EditorState
handleNormalMode currentState inputChar
  | inputChar `elem` ["i", "I"] = switchToInsertMode currentState False --TODO: Include special character "Insert"
  | inputChar `elem` ["a", "A"] = switchToInsertMode currentState True
  | inputChar == ":" = switchToCommandMode currentState
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
   in return currentState { mode = newMode, extendedPieceTable = newExtendedPieceTable, cursor = newCursor }

-- Switches to Command mode (":").
switchToCommandMode :: EditorState -> IO EditorState
switchToCommandMode currentState = return currentState { mode = Command }