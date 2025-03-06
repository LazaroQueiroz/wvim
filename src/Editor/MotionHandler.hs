module Editor.MotionHandler where

import Editor.EditorState
import Editor.ExtendedPieceTable
import Editor.Cursor
import Data.Char (isAlphaNum, isSpace)
import Data.List
import Text.Regex

import Utils


handleMotion :: EditorState -> [Char] -> IO EditorState 
handleMotion currentState inputChar = do
  command <- 
    if head inputChar `elem` "hjkl$wbxoutpnN" then 
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
  | motion == "p" = return (pasteCopyBuffer currentState)
  | motion == "t" = return (redoEditorState currentState)
  | motion == "n" = return (moveToNextRegexOccurence currentState)
  | motion == "N" = return (moveToPreviousRegexOccurence currentState)
  | head motion == 'r' = return (replaceChar currentState (last motion))
  | otherwise = return currentState

checkIfLastLineChar :: EditorState -> Bool
checkIfLastLineChar currentState = 
  let cursor' = cursor currentState
      y' = y cursor'
      x' = x cursor'
      (_, _, _, _, _, linesSizes') = extendedPieceTable currentState
  in (linesSizes' !! x' == 0)

pasteCopyBuffer :: EditorState -> EditorState
pasteCopyBuffer currentState = 
  let newUndoStack = addCurrentStateToUndoStack currentState (undoStack currentState)
      (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', linesSizes') = extendedPieceTable currentState
      newInsertStartIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
      newExtendedPieceTable = insertText (pieces', originalBuffer', addBuffer', (copyBuffer currentState), newInsertStartIndex + 1, linesSizes')
      (newPieces, newOriginalBuffer, newAddBuffer, newInsertBuffer, _, _) = newExtendedPieceTable
      currentEditorStateString = extendedPieceTableToString newExtendedPieceTable
      newLinesSizes = getLinesSizes currentEditorStateString 0 []
      newEditorStateExtendedPieceTable = (newPieces, newOriginalBuffer, newAddBuffer, newInsertBuffer, newInsertStartIndex, newLinesSizes)
  in currentState {cursor = (updateCursor 'l' (cursor currentState) newLinesSizes False), extendedPieceTable = newEditorStateExtendedPieceTable, undoStack = newUndoStack}


createNewLineBelow :: EditorState -> EditorState
createNewLineBelow currentState = 
  let newUndoStack = addCurrentStateToUndoStack currentState (undoStack currentState)
      endOfLineCursor = (cursor (moveToEndOfLine currentState True))
      (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', linesSizes') = (extendedPieceTable currentState)
      currentLineSize = linesSizes' !! (x endOfLineCursor)
      newInsertStartIndex = cursorXYToStringIndex endOfLineCursor linesSizes' 0 0
      auxiliaryExtendedPieceTable = (pieces', originalBuffer', addBuffer', insertBuffer' ++ "\n", newInsertStartIndex, linesSizes')

      (piecesFinal, originalBufferFinal, addBufferFinal, insertBufferFinal, insertStartIndexFinal, _) = insertText auxiliaryExtendedPieceTable

      newLinesSizes = updateLinesSizes "\n" endOfLineCursor linesSizes'
      newCursor = updateCursorPosition endOfLineCursor "\n" currentLineSize
      -- newCursor = (Cursor (x endOfLineCursor + 1) 0)
      newExtendedPieceTable = (piecesFinal, originalBufferFinal, addBufferFinal, insertBufferFinal, insertStartIndexFinal + 1, newLinesSizes)
  in currentState {cursor = newCursor, mode = Insert, extendedPieceTable = newExtendedPieceTable, undoStack = newUndoStack}
      

replaceChar :: EditorState -> Char -> EditorState
replaceChar currentState newChar = 
  let newUndoStack = addCurrentStateToUndoStack currentState (undoStack currentState)
      (pieces', originalBuffer', addBuffer', insertBuffer', insertStartIndex', linesSizes') = extendedPieceTable (deleteChar currentState)
      cursor' = cursor currentState
      y' = y cursor'
      x' = x cursor'
      newLinesSizes = updateLinesSizes [newChar] (cursor currentState) linesSizes'
      currentLineSize = (newLinesSizes !! x') - 1
      newInsertStartIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
      auxiliaryExtendedPieceTable = (pieces', originalBuffer', addBuffer', insertBuffer' ++ [newChar], newInsertStartIndex, newLinesSizes)
      newExtendedPieceTable = insertText auxiliaryExtendedPieceTable
      newCursor = (Cursor x' (min currentLineSize (y' + 1)))
  in currentState {cursor = newCursor, extendedPieceTable = newExtendedPieceTable, undoStack = newUndoStack}

deleteChar :: EditorState -> EditorState
deleteChar currentState
  | checkIfLastLineChar currentState = currentState
  | otherwise = 
      let newUndoStack = addCurrentStateToUndoStack currentState (undoStack currentState)
          extPieceTable = extendedPieceTable currentState
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
      in currentState {cursor = newCursor, extendedPieceTable = newExtendedPieceTable, undoStack = newUndoStack}

removeLine :: EditorState -> EditorState
removeLine currentState =
  let newUndoStack = addCurrentStateToUndoStack currentState (undoStack currentState)
      extPieceTable = extendedPieceTable currentState
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
    
  in currentState { extendedPieceTable = newExtendedPieceTable , cursor = newCursor , fileStatus = NotSaved, undoStack = newUndoStack }

undoEditorState :: EditorState -> EditorState
undoEditorState currentState = 
  let undoStack' = undoStack currentState
      redoStack' = redoStack currentState

      newRedoStack
        | length undoStack' == 0 = redoStack' 
        | otherwise = addCurrentStateToRedoStack currentState redoStack'

      (newUndoStack, oldEditorState) = 
        if null undoStack' 
        then ([], currentState) 
        else (init undoStack', last undoStack')

  in oldEditorState { mode = Normal , undoStack = newUndoStack , redoStack = newRedoStack }

redoEditorState :: EditorState -> EditorState
redoEditorState currentState = 
  let undoStack' = undoStack currentState
      redoStack' = redoStack currentState

      -- Verifica se o Redo Stack está vazio
      (newRedoStack, oldEditorState) = 
        if null redoStack' 
        then ([], currentState) 
        else (tail redoStack', head redoStack')

      -- Adiciona o estado atual ao Undo Stack
      newUndoStack 
        | length redoStack' == 0 = undoStack'
        | otherwise = addCurrentStateToUndoStack currentState undoStack'

  in oldEditorState { mode = Normal , undoStack = newUndoStack , redoStack = newRedoStack }      

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

moveToNextRegexOccurence :: EditorState -> EditorState
moveToNextRegexOccurence currentState
  | searchBuffer currentState == "" = currentState
  | otherwise = 
    let fullText = extendedPieceTableToString (extendedPieceTable currentState)
        (_, _, _, _, _, linesSizes') = (extendedPieceTable currentState)
        stringIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
        remainingText = (drop stringIndex fullText)
        regex = searchBuffer currentState
        newCursor
          | not (isInfixOf regex (drop 1 remainingText)) = cursor currentState
          | otherwise = iterateToNextRegexOccurence regex (tail remainingText) (updateCursor 'l' (cursor currentState) linesSizes' False)
    in currentState {cursor = newCursor}

moveToPreviousRegexOccurence :: EditorState -> EditorState
moveToPreviousRegexOccurence currentState
  | searchBuffer currentState == "" = currentState
  | otherwise = 
    let fullText = extendedPieceTableToString (extendedPieceTable currentState)
        (_, _, _, _, _, linesSizes') = (extendedPieceTable currentState)
        stringIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
        previousText = (take stringIndex fullText)
        regex = searchBuffer currentState
        newCursor
          | not (isInfixOf regex (init previousText)) = cursor currentState
          | otherwise = iterateToPreviousRegexOccurence regex previousText (cursor currentState) linesSizes'
    in currentState {cursor = newCursor}

iterateToNextRegexOccurence :: String -> String -> Cursor -> Cursor
iterateToNextRegexOccurence _ "" cursor = cursor
iterateToNextRegexOccurence regex text cursor'
  | take (length regex) text == regex = cursor'
  | otherwise =
    if (head text) `elem` "\r\n" then iterateToNextRegexOccurence regex (tail text) (Cursor ((x cursor') + 1) 0)
    else iterateToNextRegexOccurence regex (tail text) (Cursor (x cursor') (y cursor' + 1))

iterateToPreviousRegexOccurence :: String -> String -> Cursor -> [Int] -> Cursor
iterateToPreviousRegexOccurence _ "" cursor' _ = cursor'
iterateToPreviousRegexOccurence regex text cursor' linesSizes'
  | drop (abs (length text - length regex)) text == regex = (Cursor (x cursor') (y cursor' - (length regex)))
  | otherwise =
    if (last text) `elem` "\r\n" then iterateToPreviousRegexOccurence regex (init text) (Cursor (x cursor' - 1) (linesSizes' !! (x cursor' - 1))) linesSizes'
    else iterateToPreviousRegexOccurence regex (init text) (Cursor (x cursor') (y cursor' - 1)) linesSizes'

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



