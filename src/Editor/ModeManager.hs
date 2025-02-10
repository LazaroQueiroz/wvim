module Editor.ModeManager where

import Editor.Cursor
import Editor.EditorState
import Editor.ExtendedPieceTable
import System.Console.ANSI ()
import System.Exit (exitSuccess)
import System.IO ()
import Editor.FileManager
import Utils


handleMode :: EditorState -> [Char] -> IO EditorState
handleMode currentState inputChar =
    case mode currentState of
        Normal  -> handleNormalMode currentState inputChar
        Visual  -> handleNormalMode currentState inputChar
        Insert  -> handleInsertMode currentState inputChar
        Replace -> handleReplaceMode currentState inputChar
        Command -> handleCommandMode currentState inputChar
        _       -> return currentState

-- Handles user input in Normal mode, updating the editor state accordingly.
handleNormalMode :: EditorState -> [Char] -> IO EditorState
handleNormalMode currentState inputChar
  | inputChar `elem` ["i", "I","\ESC[2~"] = switchToInsertMode currentState False -- Switch to Insert Mode
  | inputChar `elem` ["a", "A"] = switchToInsertMode currentState True            -- Switch to Insert Mode (Alternative)
  | inputChar `elem` ["r","R"] = switchToReplaceMode currentState                 -- Switch to Replace Mode
  | inputChar `elem` ["v","V"] = switchToVisualMode currentState                  -- Switch to Visual Mode
  | inputChar == ":" = switchToCommandMode currentState                           -- Switch to Command mode
  | inputChar == "\DC2" = return currentState                                     -- TODO: REDO
  | inputChar `elem` ["u","U"] = return currentState                              -- TODO: UNDO
  | inputChar == "\DEL" = return (updateEditorStateCursor currentState "h")       -- Movement (Alternative)
  | otherwise = return (updateEditorStateCursor currentState inputChar)           -- Movement

-- Handles user input in Replace mode, updating the editor state accordingly.
handleReplaceMode :: EditorState -> [Char] -> IO EditorState
handleReplaceMode currentState inputChar
  | inputChar == "\ESC" = switchToNormalMode currentState           -- Switch to Normal mode
  | inputChar == "\ESC[2~" = switchToInsertMode currentState False  -- Switch to Replace Mode
  | inputChar == "\n" = handleInsert currentState inputChar         -- Just do it
  | inputChar == "\DEL" = handleBackspace
  | otherwise = handleReplace currentState inputChar
  where
    (pieces, originalBuffer, addBuffer, insertBuffer, _, lineSizes) = extendedPieceTable currentState
    handleBackspace
      | not (null insertBuffer) = handleDelete currentState
      | otherwise = let 
          newCursor = updateCursor 'h' (cursor currentState) lineSizes True
          newInsertStartIndex = cursorXYToStringIndex newCursor lineSizes 0 0
          newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, lineSizes)
       in return currentState {cursor = newCursor, extendedPieceTable = newExtendedPieceTable}

-- Handles user input in Insert mode, updating the editor state accordingly.
handleInsertMode :: EditorState -> [Char] -> IO EditorState
handleInsertMode currentState inputChar
  | inputChar == "\ESC" = switchToNormalMode currentState           -- Switch to Normal mode
  | inputChar == "\ESC[2~" = switchToReplaceMode currentState       -- Switch to Replace Mode
  | inputChar == "\ESC[3~" = return currentState                    -- TODO: Delete character on cursor
  | inputChar == "\DEL" && x' == 0 && y' == 0 = return currentState -- Don't delete character 
  | inputChar == "\DEL" = handleDelete currentState                 -- Delete character before cursor
  | otherwise = handleInsert currentState inputChar                 -- Inserts character
  where
    Cursor x' y' = cursor currentState

-- Handles user input in Command mode, updating the editor state accordingly..
handleCommandMode :: EditorState -> String -> IO EditorState
handleCommandMode state inputChar
  | inputChar == "\ESC" = return state {mode = Normal, commandText = ""}
  | inputChar == "\DEL" = deleteCommandText
  | inputChar == "\n" = handleCommand state (commandText state)
  | otherwise = return $ state { commandText = commandText state ++ inputChar }
  where
    deleteCommandText
     | null (commandText state) = return state
     | otherwise = return $ state { commandText = init (commandText state) }

-- Handles the command when the user presses "Enter"
handleCommand :: EditorState -> String -> IO EditorState
handleCommand state inputString
  | command == "w"   = saveFile state False args
  | command == "w!"  = saveFile state True args
  | command == "q"   = quitEditor state
  | command == "q!"  = exitSuccess
  | command == "wq"  = saveAndQuit state False args
  | command == "wq!" = saveAndQuit state True args
  | otherwise        = return $ setError state "Command not found."
  where
    (command, rawArgs) = break (== ' ') inputString
    args = dropWhile (== ' ') rawArgs

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

-- Handles character replacement
handleReplace :: EditorState -> [Char] -> IO EditorState
handleReplace currentState inputChar = do
  let extPieceTable = extendedPieceTable currentState
      (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = extPieceTable
      newInsertBuffer = insertBuffer ++ inputChar
      newCursor = updateCursorPosition (cursor currentState) inputChar 0
      Cursor x' y' = newCursor
      tempPieceTable = (pieces, originalBuffer, addBuffer, newInsertBuffer, insertStartIndex, linesSizes)
      newExtendedPieceTable
        | (linesSizes !! x') < y' = 
            let newLinesSizes = updateLinesSizes inputChar (cursor currentState) linesSizes
             in (pieces, originalBuffer, addBuffer, newInsertBuffer, insertStartIndex, newLinesSizes)
        | otherwise =
            let (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', newLinesSizes) = deleteText (insertStartIndex + 1) 1 tempPieceTable
             in (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex' + 1, newLinesSizes)
   in return currentState { extendedPieceTable = newExtendedPieceTable, cursor = newCursor, fileStatus = NotSaved }

-- Switches to Normal mode ('\ESC')
switchToNormalMode :: EditorState -> IO EditorState
switchToNormalMode currentState = do
  let newMode = Normal
      extPieceTable = extendedPieceTable currentState
      newExtendedPieceTable = insertText extPieceTable
      (_, _, _, _, _, linesSizes') = newExtendedPieceTable
      newCursor = updateCursor 'h' (cursor currentState) linesSizes' False
   in return currentState { mode = newMode, extendedPieceTable = newExtendedPieceTable, cursor = newCursor }

-- Switches to Replace mode ('r' or 'R'), optionally while on Insert by pressing the special char "Insert".
switchToReplaceMode :: EditorState -> IO EditorState
switchToReplaceMode currentState = do
  let newMode = Replace
      (pieces, originalBuffer, addBuffer, insertBuffer, _, lineSizes) = insertText (extendedPieceTable currentState)
      newCursor = updateCursor 'R' (cursor currentState) lineSizes True
      newInsertStartIndex = cursorXYToStringIndex newCursor lineSizes 0 0
      newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, lineSizes)
   in return currentState { mode = newMode, extendedPieceTable = newExtendedPieceTable, cursor = newCursor }

-- Switches to Insert mode ('i' or 'I'), optionally moving the cursor forward ('a' or 'A').
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

-- Switches to Command mode (':').
switchToCommandMode :: EditorState -> IO EditorState
switchToCommandMode currentState = return currentState { mode = Command }

-- Switches to Command mode (':').
switchToVisualMode :: EditorState -> IO EditorState
switchToVisualMode currentState = return currentState { mode = Visual }