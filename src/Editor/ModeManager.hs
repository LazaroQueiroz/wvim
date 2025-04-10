module Editor.ModeManager where

import Editor.Cursor
import Editor.EditorState
import Editor.MotionHandler
import Editor.ExtendedPieceTable
import Editor.FileManager
import Editor.Viewport
import Text.Regex (mkRegex, subRegex)
import Utils

handleMode :: EditorState -> [Char] -> IO EditorState
handleMode currentState inputChar =
  case mode currentState of
    Normal -> handleNormalMode currentState inputChar
    Visual -> handleVisualMode currentState inputChar
    Insert -> handleInsertMode currentState inputChar
    Replace -> handleReplaceMode currentState inputChar
    Substitution -> handleSubstitutionMode currentState inputChar
    Command -> handleCommandMode currentState inputChar
    _ -> return currentState

handleSearch :: EditorState -> [Char] -> IO EditorState
handleSearch currentState searchBuffer' = do
  let segments = wordsWhen (== '/') searchBuffer'
  if length segments == 1 then do
    return currentState { mode = Normal, searchBuffer = head segments }
  else if  length segments == 2 then do
    return (replaceRegex currentState segments)
  else
    return currentState

-- Handles user input in Command mode, updating the editor state accordingly..
handleSubstitutionMode :: EditorState -> String -> IO EditorState
handleSubstitutionMode state inputChar
  | inputChar == "\ESC" = return state {mode = Normal, searchBuffer = ""}
  | inputChar == "\DEL" = deleteSearchText
  | inputChar == "\n" = handleSearch state (searchBuffer state)
  | otherwise = return $ state {searchBuffer = searchBuffer state ++ inputChar}
  where
    deleteSearchText
      | null (searchBuffer state) = return state
      | otherwise = return $ state {searchBuffer = init (searchBuffer state), fileStatus = NotSaved}

replaceRegex :: EditorState -> [String] -> EditorState
replaceRegex currentState segments =
  let newUndoStack = addCurrentStateToUndoStack currentState (undoStack currentState)
      extendedPieceTable' = extendedPieceTable currentState
      currentEditorStateString = extendedPieceTableToString extendedPieceTable'
      newEditorStateString = subRegex (mkRegex (head segments)) currentEditorStateString (segments !! 1)
      (pieces', originalBuffer', addBuffer', insertBuffer', _, linesSizes') = createExtendedPieceTable newEditorStateString
      newInsertStartIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
      newExtendedPieceTable = (pieces', originalBuffer', addBuffer', insertBuffer', newInsertStartIndex, linesSizes')
  in currentState { mode = Normal, extendedPieceTable = newExtendedPieceTable, searchBuffer = "", undoStack = newUndoStack }

handleVisualMode :: EditorState -> [Char] -> IO EditorState
handleVisualMode currentState inputChar
  | inputChar == "\ESC" = return (currentState {mode = Normal})
  | inputChar == "v" = do
    let (_, _, _, _, _, linesSizes') = extendedPieceTable currentState
        currentCursorStringIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
        extendedPieceTable' = extendedPieceTable currentState
        currentEditorStateString = extendedPieceTableToString extendedPieceTable'
        visualModeStartIndex' = visualModeStartIndex currentState
        newCopyBuffer = take (abs (visualModeStartIndex' - currentCursorStringIndex) + 1) (drop (min visualModeStartIndex' currentCursorStringIndex) currentEditorStateString)
    return currentState {mode = Normal, copyBuffer = newCopyBuffer}
  | otherwise = handleMotion currentState inputChar

-- Handles user input in Normal mode, updating the editor state accordingly.
handleNormalMode :: EditorState -> [Char] -> IO EditorState
handleNormalMode currentState inputChar
  | inputChar `elem` ["i", "I"] = switchMode currentState Insert False -- Switch to Insert Mode
  | inputChar `elem` ["a", "A"] = switchMode currentState Insert True -- Switch to Insert Mode (Alternative)
  | inputChar `elem` ["v", "V"] = switchMode currentState Visual False -- Switch to Visual Mode
  | inputChar == "R" = switchMode currentState Replace False -- Switch to Replace Mode
  | inputChar == ":" = switchMode currentState Command False -- Switch to Command mode
  | inputChar == "/" = switchMode currentState {searchBuffer = ""} Substitution False -- Switch to Command mode
  | otherwise = handleMotion currentState inputChar

-- Handles user input in Replace mode, updating the editor state accordingly.
handleReplaceMode :: EditorState -> [Char] -> IO EditorState
handleReplaceMode currentState inputChar
  | inputChar == "\ESC" = switchMode currentState Normal False -- Switch to Normal mode
  | inputChar == "\n" = handleInsert currentState inputChar -- Just do it
  | inputChar == "\DEL" = handleBackspace
  | otherwise = handleReplace currentState inputChar
  where
    (pieces, originalBuffer, addBuffer, insertBuffer, _, lineSizes) = extendedPieceTable currentState
    handleBackspace
      | not (null insertBuffer) = handleDelete currentState
      | otherwise =
          let newCursor = updateCursor 'h' (cursor currentState) lineSizes True
              newInsertStartIndex = cursorXYToStringIndex newCursor lineSizes 0 0
              newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, lineSizes)
           in return currentState {cursor = newCursor, extendedPieceTable = newExtendedPieceTable}

-- Handles user input in Insert mode, updating the editor state accordingly.
handleInsertMode :: EditorState -> [Char] -> IO EditorState
handleInsertMode currentState inputChar
  | inputChar == "\ESC" = switchMode currentState Normal False -- Switch to Normal mode
  | inputChar == "\DEL" && x' == 0 && y' == 0 && initialColumn (viewport currentState) == 0 && initialRow (viewport currentState) == 0 = return currentState -- Don't delete character
  | inputChar == "\DEL" = handleDelete currentState -- Delete character before cursor
  | otherwise = handleInsert currentState inputChar -- Inserts character
  where
    Cursor x' y' = cursor currentState

-- Handles user input in Command mode, updating the editor state accordingly..
handleCommandMode :: EditorState -> String -> IO EditorState
handleCommandMode state inputChar
  | inputChar == "\ESC" = return state {mode = Normal, commandBuffer = ""}
  | inputChar == "\DEL" = deletecommandBuffer
  | inputChar == "\n" = handleCommand state (commandBuffer state)
  | otherwise = return $ state {commandBuffer = commandBuffer state ++ inputChar}
  where
    deletecommandBuffer
      | null (commandBuffer state) = return state
      | otherwise = return $ state {commandBuffer = init (commandBuffer state)}

-- Handles the command when the user presses "Enter"
handleCommand :: EditorState -> String -> IO EditorState
handleCommand state inputString
  | command == "w" = saveFile state False args
  | command == "w!" = saveFile state True args
  | command == "q" = quitEditor state
  | command == "q!" = return state {mode = Closed}
  | command == "wq" = saveAndQuit state False args
  | command == "wq!" = saveAndQuit state True args
  | otherwise = return $ setError state "Command not found."
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
   in return currentState {extendedPieceTable = newExtendedPieceTable, cursor = newCursor, fileStatus = NotSaved} -- CVH

-- Handles character deletion
handleDelete :: EditorState -> IO EditorState
handleDelete currentState = do
  let extPieceTable = extendedPieceTable currentState
      (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) = extPieceTable
      Cursor x' _ = cursor currentState
      newLinesSizes = updateLinesSizes "\DEL" (cursor currentState) linesSizes
      newCursor = updateCursorPosition (cursor currentState) "\DEL" (linesSizes !! (x' - 1))
      newExtendedPieceTable
        | not (null insertBuffer) =
            let newInsertBuffer = take (length insertBuffer - 1) insertBuffer
             in (pieces, originalBuffer, addBuffer, newInsertBuffer, insertStartIndex, newLinesSizes)
        | otherwise =
            let (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', _) = deleteText insertStartIndex 1 extPieceTable
             in (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', newLinesSizes)
   in return currentState {extendedPieceTable = newExtendedPieceTable, cursor = newCursor, fileStatus = NotSaved} -- CVH

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
   in return currentState {extendedPieceTable = newExtendedPieceTable, cursor = newCursor, fileStatus = NotSaved}

-- Switches the editor state mode, updating it accordingly.
switchMode :: EditorState -> Mode -> Bool -> IO EditorState
switchMode currentState newMode moveCursor =
  case newMode of
    Normal ->
      let extPieceTable = extendedPieceTable currentState
          newExtendedPieceTable = insertText extPieceTable
          (_, _, _, _, _, linesSizes') = newExtendedPieceTable
          newCursor = updateCursor 'h' (cursor currentState) linesSizes' False
       in return currentState {mode = newMode, extendedPieceTable = newExtendedPieceTable, cursor = newCursor} -- CVH
    Insert ->
      let (pieces, originalBuffer, addBuffer, insertBuffer, _, linesSizes) = extendedPieceTable currentState
          newCursor
            | moveCursor = updateCursor 'l' (cursor currentState) linesSizes True
            | otherwise = cursor currentState
          newInsertStartIndex = cursorXYToStringIndex newCursor linesSizes 0 0
          newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, linesSizes)
          undoStack' = undoStack currentState
          previousEditorState
            | null undoStack' = currentState
            | otherwise = last undoStack'
          previousEditorStateExtendedPieceTable = extendedPieceTable previousEditorState
          previousEditorStateString
            | null undoStack' = "*"
            | otherwise = extendedPieceTableToString previousEditorStateExtendedPieceTable
          currentExtendedPieceTable = insertText newExtendedPieceTable
          currentEditorStateString = extendedPieceTableToString currentExtendedPieceTable
          newUndoStack
            | currentEditorStateString == previousEditorStateString = undoStack'
            | otherwise = addCurrentStateToUndoStack currentState undoStack'
       in return currentState {mode = newMode, extendedPieceTable = newExtendedPieceTable, cursor = newCursor, undoStack = newUndoStack, redoStack = []}

    Replace ->
      let (pieces, originalBuffer, addBuffer, insertBuffer, _, lineSizes) = insertText (extendedPieceTable currentState)
          newCursor = updateCursor 'R' (cursor currentState) lineSizes True
          newInsertStartIndex = cursorXYToStringIndex newCursor lineSizes 0 0
          newExtendedPieceTable = (pieces, originalBuffer, addBuffer, insertBuffer, newInsertStartIndex, lineSizes)
       in return currentState {mode = newMode, extendedPieceTable = newExtendedPieceTable, cursor = newCursor}
    Command -> return currentState {mode = Command}
    Visual ->
      let (_, _, _, _, _, linesSizes') = extendedPieceTable currentState
          currentCursorStringIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
      in return currentState {mode = Visual, visualModeStartIndex = currentCursorStringIndex}
    Substitution -> return currentState { mode = Substitution }
    _ -> return currentState
