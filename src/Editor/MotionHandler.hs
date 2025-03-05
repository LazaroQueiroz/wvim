module Editor.MotionHandler where

import Editor.EditorState
import Editor.ExtendedPieceTable
import Editor.Cursor
import Data.Char (isAlphaNum, isSpace)

import Utils


handleMotion :: EditorState -> [Char] -> IO EditorState 
handleMotion currentState inputChar = do
  command <- 
    if head inputChar `elem` "hjkl$wbxout" then 
      return inputChar
    else do
      restInput <- getRemainingInput inputChar
      return (inputChar ++ restInput)

  let (multiplier, motion) = getLeadingNumberAndRest command
  newEditorState <- runCommand currentState multiplier motion
  return newEditorState

runCommand :: EditorState -> Int -> String -> IO EditorState
runCommand currentState 0 _ = return currentState
runCommand currentState multiplier motion = do
    nextEditorState <- runMotion currentState motion
    runCommand nextEditorState (multiplier - 1) motion

runMotion :: EditorState -> [Char] -> IO EditorState
runMotion currentState motion
  | head motion `elem` "hjkl" = return (updateEditorStateCursor currentState motion)
  | motion == "w" = return (moveToNextWord currentState)
  | motion == "b" = return (moveToPreviousWord currentState)
  | motion == "$" = return (moveToEndOfLine currentState False)
  | motion == "dd" = return (removeLine currentState)
  | motion == "x" = return (deleteChar currentState)
  | motion == "o" = return (createNewLineBelow currentState)
  | motion == "u" = return (undoEditorState currentState)
  | motion == "t" = return (redoEditorState currentState)
  | head motion == 'r' = return (replaceChar currentState (last motion))
  | otherwise = return currentState

checkIfLastLineChar :: EditorState -> Bool
checkIfLastLineChar currentState = 
  let cursor' = cursor currentState
      y' = y cursor'
      x' = x cursor'
      (_, _, _, _, _, linesSizes') = extendedPieceTable currentState
  in (linesSizes' !! x' == 0)

createNewLineBelow :: EditorState -> EditorState
createNewLineBelow currentState = 
  let endOfLineCursor = (cursor (moveToEndOfLine currentState True))
      (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', linesSizes') = (extendedPieceTable currentState)
      currentLineSize = linesSizes' !! (x endOfLineCursor)
      newInsertStartIndex = cursorXYToStringIndex endOfLineCursor linesSizes' 0 0
      auxiliaryExtendedPieceTable = (pieces', originalBuffer', addBuffer', insertBuffer' ++ "\n", newInsertStartIndex, linesSizes')

      (piecesFinal, originalBufferFinal, addBufferFinal, insertBufferFinal, insertStartIndexFinal, _) = insertText auxiliaryExtendedPieceTable

      newLinesSizes = updateLinesSizes "\n" endOfLineCursor linesSizes'
      newCursor = updateCursorPosition endOfLineCursor "\n" currentLineSize
      -- newCursor = (Cursor (x endOfLineCursor + 1) 0)
      newExtendedPieceTable = (piecesFinal, originalBufferFinal, addBufferFinal, insertBufferFinal, insertStartIndexFinal + 1, newLinesSizes)
  in currentState {cursor = newCursor, mode = Insert, extendedPieceTable = newExtendedPieceTable}
      

replaceChar :: EditorState -> Char -> EditorState
replaceChar currentState newChar = 
  let (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', linesSizes') = extendedPieceTable (deleteChar currentState)
      cursor' = cursor currentState
      y' = y cursor'
      x' = x cursor'
      newLinesSizes = updateLinesSizes [newChar] (cursor currentState) linesSizes'
      currentLineSize = (newLinesSizes !! x') - 1
      newInsertStartIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
      auxiliaryExtendedPieceTable = (pieces', originalBuffer', addBuffer', insertBuffer' ++ [newChar], newInsertStartIndex, newLinesSizes)
      newExtendedPieceTable = insertText auxiliaryExtendedPieceTable
      newCursor = (Cursor x' (min currentLineSize (y' + 1)))
  in currentState {cursor = newCursor, extendedPieceTable = newExtendedPieceTable}

deleteChar :: EditorState -> EditorState
deleteChar currentState
  | checkIfLastLineChar currentState = currentState
  | otherwise = 
      let extPieceTable = extendedPieceTable currentState
          cursor' = (cursor currentState)
          y' = y cursor'
          x' = x cursor'
          (_, _, _, _, _, linesSizes') = (extendedPieceTable currentState)
          deleteStartIndex = (cursorXYToStringIndex cursor' linesSizes' 0 0) 
          (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', _) = deleteText (deleteStartIndex + 1) 1 extPieceTable
          newLinesSizes = updateLinesSizes "\DEL" (Cursor x' (y' + 1)) linesSizes'
          newExtendedPieceTable = (pieces', originalBuffer', addBuffer', insertBuffer', deleteStartIndex, newLinesSizes)
          newCursor
            | y' >= (linesSizes' !! x') - 1 = Cursor x' (max 0 (y' - 1))
            | otherwise = cursor'
      in currentState {cursor = newCursor, extendedPieceTable = newExtendedPieceTable}

removeLine :: EditorState -> EditorState
removeLine currentState =
  let extPieceTable = extendedPieceTable currentState
      (_, _, _, _, _, linesSizes') = extPieceTable
      Cursor x' _ = cursor currentState
      
      lineStartIndex = sum (take x' linesSizes') + x' + (if not (x' == length linesSizes' - 1) then 1 else 0)
      lineLength = linesSizes' !! x' + (if not (length linesSizes' == 1) then 1 else 0)
      lineEndIndex = lineStartIndex + lineLength
      
      newLinesSizes = 
        if length linesSizes' == 1 then [0]
        else take x' linesSizes' ++ drop (x' + 1) linesSizes'
      
      (newPieces, newOriginalBuffer, newAddBuffer, newInsertBuffer, newInsertStartIndex, _) =
          deleteText lineStartIndex lineLength extPieceTable
      
      newCursor = if x' == length linesSizes' - 1 then Cursor (max 0 (x' - 1)) 0 else (Cursor x' 0)
      
      newExtendedPieceTable = 
          (newPieces, newOriginalBuffer, newAddBuffer, newInsertBuffer, newInsertStartIndex, newLinesSizes)
    
  in currentState { extendedPieceTable = newExtendedPieceTable , cursor = newCursor , fileStatus = NotSaved }

undoEditorState :: EditorState -> EditorState
undoEditorState currentState = 
  let undoStack' = undoStack currentState
      redoStack' = redoStack currentState

      newRedoStack = addCurrentStateToRedoStack (last undoStack') redoStack'

      -- isso aqui funciona, stack diminui de tamanho
      newUndoStack = if null undoStack' then [] else init undoStack'

      -- pega ultima coisa do coisa
      -- (EditorState oldMode oldExtendedPieceTable oldCursor oldViewport oldFileStatus oldFilename oldStatusBar oldCommandText oldUndoStack' oldRedoStack') = if null undoStack' then currentState else last undoStack'
      oldEditorState
        | null newUndoStack = currentState
        | otherwise = last newUndoStack

  in oldEditorState {mode = Normal, undoStack = newUndoStack, redoStack = newRedoStack}

redoEditorState :: EditorState -> EditorState
redoEditorState currentState = 
  let undoStack' = undoStack currentState
      redoStack' = redoStack currentState

      newUndoStack = addCurrentStateToUndoStack currentState redoStack'

      -- isso aqui funciona, stack diminui de tamanho
      newRedoStack = if null redoStack' then [] else init redoStack'

      -- pega ultima coisa do coisa
      -- (EditorState oldMode oldExtendedPieceTable oldCursor oldViewport oldFileStatus oldFilename oldStatusBar oldCommandText oldUndoStack' oldRedoStack') = if null undoStack' then currentState else last undoStack'
      oldEditorState
        | null newRedoStack = currentState
        | otherwise = last newRedoStack

  in oldEditorState {mode = Normal, undoStack = newUndoStack, redoStack = newRedoStack}
      

moveToEndOfLine :: EditorState -> Bool -> EditorState
moveToEndOfLine currentState insertModeOn = 
  let cursor' = (cursor currentState)
      (_, _, _, _, _, linesSizes') = (extendedPieceTable currentState)
      x' = x cursor'
      currentLineSize = linesSizes' !! x'
      newCursor = (Cursor x' (currentLineSize - (if insertModeOn then 0 else 1)))
  in currentState {cursor = newCursor}

moveToNextWord :: EditorState -> EditorState
moveToNextWord currentState =
  let fullText = extendedPieceTableToString (extendedPieceTable currentState)
      (_, _, _, _, _, linesSizes') = (extendedPieceTable currentState)
      stringIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
      remainingText = drop stringIndex fullText
      newCursor = iterateToNextBlankSpace remainingText (cursor currentState)
  in currentState {cursor = newCursor}

moveToPreviousWord :: EditorState -> EditorState
moveToPreviousWord currentState =
  let fullText = extendedPieceTableToString (extendedPieceTable currentState)
      (_, _, _, _, _, linesSizes') = (extendedPieceTable currentState)
      stringIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
      previousText = take stringIndex fullText
      newCursor = iterateToPreviousWordStart previousText (cursor currentState) linesSizes'
  in currentState {cursor = newCursor}

iterateToNextBlankSpace :: String -> Cursor -> Cursor
iterateToNextBlankSpace "" cursor = cursor
iterateToNextBlankSpace (c:cs) cursor
  | c `elem` "\r\n" = Cursor ((x cursor) + 1) 0
  | c `elem` "\t " && not ((head cs) `elem` "\t ") = Cursor (x cursor) ((y cursor) + 1)
  | otherwise = iterateToNextBlankSpace cs (Cursor (x cursor) (y cursor + 1))

iterateToPreviousWordStart :: String -> Cursor -> [Int] -> Cursor
iterateToPreviousWordStart "" cursor linesSizes' = cursor
iterateToPreviousWordStart text cursor linesSizes'
  | length text == 1 = Cursor (x cursor) (y cursor - 1) -- before the last line
  | not (last text `elem` "\r\n\t ") && (last (init text) `elem` "\r\n\t ") = Cursor (x cursor) (y cursor - 1) -- the start of the word
  | last text `elem` "\r\n" && (last (init text) `elem` "\r\n") = Cursor (x cursor - 1) (linesSizes' !! ((x cursor) - 1)) -- jump to line above
  | last text `elem` "\r\n" = iterateToPreviousWordStart (init text) (Cursor (x cursor - 1) (linesSizes' !! ((x cursor) - 1))) linesSizes' -- 
  | otherwise = iterateToPreviousWordStart (init text) (Cursor (x cursor) (y cursor - 1)) linesSizes'

getRemainingInput :: String -> IO String
getRemainingInput lastChar = do
  char <- getChar
  if (char `elem` "hjkl$wbx") || not (isNumber char) && not (isNumber (head lastChar)) then
    return [char]
  else do
    rest <- getRemainingInput [char]
    return (char : rest)



