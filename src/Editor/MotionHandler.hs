module Editor.MotionHandler where

import Data.Char (isDigit)
import Data.List
import Editor.Cursor
import Editor.EditorState
import Editor.ExtendedPieceTable
import Utils

handleMotion :: EditorState -> [Char] -> IO EditorState
handleMotion currentState inputChar = do
  command <-
    if head inputChar `elem` "hjkl$wbxoutpnN^"
      then
        return inputChar
      else do
        restInput <- getRemainingInput inputChar
        return (inputChar ++ restInput)

  let (multiplier, motion) = getLeadingNumberAndRest command
  runCommand currentState multiplier motion

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
  | motion == "^" = return (moveToStartOfLine currentState False)
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
  let Cursor x' _ = cursor currentState
      (_, _, _, _, _, linesSizes') = extendedPieceTable currentState
   in (linesSizes' !! x' == 0)

pasteCopyBuffer :: EditorState -> EditorState
pasteCopyBuffer currentState =
  let newUndoStack = addCurrentStateToUndoStack currentState (undoStack currentState)
      (pieces', originalBuffer', addBuffer', _, _, linesSizes') = extendedPieceTable currentState
      newInsertStartIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
      newExtendedPieceTable = insertText (pieces', originalBuffer', addBuffer', copyBuffer currentState, newInsertStartIndex + 1, linesSizes')
      (newPieces, newOriginalBuffer, newAddBuffer, newInsertBuffer, _, _) = newExtendedPieceTable
      currentEditorStateString = extendedPieceTableToString newExtendedPieceTable
      newLinesSizes = getLinesSizes currentEditorStateString 0 []
      newEditorStateExtendedPieceTable = (newPieces, newOriginalBuffer, newAddBuffer, newInsertBuffer, newInsertStartIndex, newLinesSizes)
   in currentState {cursor = updateCursor 'l' (cursor currentState) newLinesSizes False, extendedPieceTable = newEditorStateExtendedPieceTable, undoStack = newUndoStack}

createNewLineBelow :: EditorState -> EditorState
createNewLineBelow currentState =
  let newUndoStack = addCurrentStateToUndoStack currentState (undoStack currentState)
      endOfLineCursor = cursor (moveToEndOfLine currentState True)
      (pieces', originalBuffer', addBuffer', insertBuffer', _, linesSizes') = extendedPieceTable currentState
      currentLineSize = linesSizes' !! x endOfLineCursor
      newInsertStartIndex = cursorXYToStringIndex endOfLineCursor linesSizes' 0 0
      auxiliaryExtendedPieceTable = (pieces', originalBuffer', addBuffer', insertBuffer' ++ "\n", newInsertStartIndex, linesSizes')
      (piecesFinal, originalBufferFinal, addBufferFinal, insertBufferFinal, insertStartIndexFinal, _) = insertText auxiliaryExtendedPieceTable
      newLinesSizes = updateLinesSizes "\n" endOfLineCursor linesSizes'
      newCursor = updateCursorPosition endOfLineCursor "\n" currentLineSize
      newExtendedPieceTable = (piecesFinal, originalBufferFinal, addBufferFinal, insertBufferFinal, insertStartIndexFinal + 1, newLinesSizes)
   in currentState {cursor = newCursor, mode = Insert, extendedPieceTable = newExtendedPieceTable, undoStack = newUndoStack}

replaceChar :: EditorState -> Char -> EditorState
replaceChar currentState newChar =
  let newUndoStack = addCurrentStateToUndoStack currentState (undoStack currentState)
      (pieces', originalBuffer', addBuffer', insertBuffer', _, linesSizes') = extendedPieceTable (deleteChar currentState)
      Cursor x' y' = cursor currentState
      newLinesSizes = updateLinesSizes [newChar] (cursor currentState) linesSizes'
      currentLineSize = (newLinesSizes !! x') - 1
      newInsertStartIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
      auxiliaryExtendedPieceTable = (pieces', originalBuffer', addBuffer', insertBuffer' ++ [newChar], newInsertStartIndex, newLinesSizes)
      newExtendedPieceTable = insertText auxiliaryExtendedPieceTable
      newCursor = Cursor x' (min currentLineSize (y' + 1))
   in currentState {cursor = newCursor, extendedPieceTable = newExtendedPieceTable, undoStack = newUndoStack}

deleteChar :: EditorState -> EditorState
deleteChar currentState
  | checkIfLastLineChar currentState = currentState
  | otherwise =
      let newUndoStack = addCurrentStateToUndoStack currentState (undoStack currentState)
          extPieceTable = extendedPieceTable currentState
          Cursor x' y' = cursor currentState
          (_, _, _, _, _, linesSizes') = extendedPieceTable currentState
          deleteStartIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
          (pieces', originalBuffer', addBuffer', insertBuffer', _, _) = deleteText (deleteStartIndex + 1) 1 extPieceTable
          newLinesSizes = updateLinesSizes "\DEL" (Cursor x' (y' + 1)) linesSizes'
          newExtendedPieceTable = (pieces', originalBuffer', addBuffer', insertBuffer', deleteStartIndex, newLinesSizes)
          newCursor
            | y' >= (linesSizes' !! x') - 1 = Cursor x' (max 0 (y' - 1))
            | otherwise = cursor currentState
       in currentState {cursor = newCursor, extendedPieceTable = newExtendedPieceTable, undoStack = newUndoStack}

removeLine :: EditorState -> EditorState
removeLine currentState =
  let newUndoStack = addCurrentStateToUndoStack currentState (undoStack currentState)
      extPieceTable = extendedPieceTable currentState
      (_, _, _, _, _, linesSizes') = extPieceTable
      Cursor x' _ = cursor currentState

      lineStartIndex = sum (take x' linesSizes') + x' + (if x' /= (length linesSizes' - 1) then 1 else 0)
      lineLength = linesSizes' !! x' + (if length linesSizes' /= 1 then 1 else 0)

      newLinesSizes
        | length linesSizes' == 1 = [0]
        | otherwise = take x' linesSizes' ++ drop (x' + 1) linesSizes'

      (newPieces, newOriginalBuffer, newAddBuffer, newInsertBuffer, newInsertStartIndex, _) =
        deleteText lineStartIndex lineLength extPieceTable

      newCursor
        | x' == length linesSizes' - 1 = Cursor (max 0 (x' - 1)) 0
        | otherwise = Cursor x' 0

      newExtendedPieceTable =
        (newPieces, newOriginalBuffer, newAddBuffer, newInsertBuffer, newInsertStartIndex, newLinesSizes)
   in currentState {extendedPieceTable = newExtendedPieceTable, cursor = newCursor, fileStatus = NotSaved, undoStack = newUndoStack}

undoEditorState :: EditorState -> EditorState
undoEditorState currentState =
  let undoStack' = undoStack currentState
      redoStack' = redoStack currentState

      newRedoStack
        | null undoStack' = redoStack'
        | otherwise = addCurrentStateToRedoStack currentState redoStack'

      (newUndoStack, oldEditorState)
        | null undoStack' = ([], currentState)
        | otherwise = (init undoStack', last undoStack')
   in oldEditorState {mode = Normal, undoStack = newUndoStack, redoStack = newRedoStack}

redoEditorState :: EditorState -> EditorState
redoEditorState currentState =
  let undoStack' = undoStack currentState
      redoStack' = redoStack currentState

      -- Verifica se o Redo Stack está vazio
      (newRedoStack, oldEditorState)
        | null redoStack' = ([], currentState)
        | otherwise = (tail redoStack', head redoStack')

      -- Adiciona o estado atual ao Undo Stack
      newUndoStack
        | null redoStack' = undoStack'
        | otherwise = addCurrentStateToUndoStack currentState undoStack'
   in oldEditorState {mode = Normal, undoStack = newUndoStack, redoStack = newRedoStack}

moveToEndOfLine :: EditorState -> Bool -> EditorState
moveToEndOfLine currentState insertModeOn =
  let cursor' = cursor currentState
      (_, _, _, _, _, linesSizes') = extendedPieceTable currentState
      x' = x cursor'
      currentLineSize = linesSizes' !! x'
      newCursor = Cursor x' (currentLineSize - (if insertModeOn then 0 else 1))
   in currentState {cursor = newCursor}

moveToStartOfLine :: EditorState -> Bool -> EditorState
moveToStartOfLine currentState insertModeOn =
  let cursor' = cursor currentState
      x' = x cursor'
      newCursor = Cursor x' 0
   in currentState {cursor = newCursor}

moveToNextWord :: EditorState -> EditorState
moveToNextWord currentState =
  let fullText = extendedPieceTableToString (extendedPieceTable currentState)
      (_, _, _, _, _, linesSizes') = extendedPieceTable currentState
      stringIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
      remainingText = drop stringIndex fullText
      newCursor = iterateToNextBlankSpace remainingText (cursor currentState)
   in currentState {cursor = newCursor}

moveToPreviousWord :: EditorState -> EditorState
moveToPreviousWord currentState =
  let fullText = extendedPieceTableToString (extendedPieceTable currentState)
      (_, _, _, _, _, linesSizes') = extendedPieceTable currentState
      stringIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
      previousText = take stringIndex fullText
      newCursor = iterateToPreviousWordStart previousText (cursor currentState) linesSizes'
   in currentState {cursor = newCursor}

moveToNextRegexOccurence :: EditorState -> EditorState
moveToNextRegexOccurence currentState
  | searchBuffer currentState == "" = currentState
  | otherwise =
      let fullText = extendedPieceTableToString (extendedPieceTable currentState)
          (_, _, _, _, _, linesSizes') = extendedPieceTable currentState
          stringIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
          remainingText = drop stringIndex fullText
          regex = searchBuffer currentState
          newCursor
            | not (regex `isInfixOf` drop 1 remainingText) = cursor currentState
            | otherwise = iterateToNextRegexOccurence regex (tail remainingText) (updateCursor 'l' (cursor currentState) linesSizes' False)
       in currentState {cursor = newCursor}

moveToPreviousRegexOccurence :: EditorState -> EditorState
moveToPreviousRegexOccurence currentState
  | searchBuffer currentState == "" = currentState
  | otherwise =
      let fullText = extendedPieceTableToString (extendedPieceTable currentState)
          (_, _, _, _, _, linesSizes') = extendedPieceTable currentState
          stringIndex = cursorXYToStringIndex (cursor currentState) linesSizes' 0 0
          previousText = take stringIndex fullText
          regex = searchBuffer currentState
          newCursor
            | not (regex `isInfixOf` safeInit previousText) = cursor currentState
            | otherwise = iterateToPreviousRegexOccurence regex previousText (cursor currentState) linesSizes'
       in currentState {cursor = newCursor}

iterateToNextRegexOccurence :: String -> String -> Cursor -> Cursor
iterateToNextRegexOccurence _ "" cursor' = cursor'
iterateToNextRegexOccurence regex text cursor'
  | take (length regex) text == regex = cursor'
  | otherwise =
      if head text `elem` "\r\n"
        then iterateToNextRegexOccurence regex (tail text) (Cursor (x cursor' + 1) 0)
        else iterateToNextRegexOccurence regex (tail text) (Cursor (x cursor') (y cursor' + 1))

iterateToPreviousRegexOccurence :: String -> String -> Cursor -> [Int] -> Cursor
iterateToPreviousRegexOccurence _ "" cursor' _ = cursor'
iterateToPreviousRegexOccurence regex text cursor' linesSizes'
  | drop (abs (length text - length regex)) text == regex = Cursor (x cursor') (y cursor' - length regex)
  | otherwise =
      if last text `elem` "\r\n"
        then iterateToPreviousRegexOccurence regex (init text) (Cursor (x cursor' - 1) (linesSizes' !! (x cursor' - 1))) linesSizes'
        else iterateToPreviousRegexOccurence regex (init text) (Cursor (x cursor') (y cursor' - 1)) linesSizes'

iterateToNextBlankSpace :: [Char] -> Cursor -> Cursor
iterateToNextBlankSpace "" cursor' = cursor'
iterateToNextBlankSpace (c : cs) cursor'
  | c `elem` "\r\n" = Cursor (x cursor' + 1) 0
  | c `elem` "\t " && notElem (head cs) "\t " = Cursor (x cursor') (y cursor' + 1)
  | otherwise = iterateToNextBlankSpace cs (Cursor (x cursor') (y cursor' + 1))

iterateToPreviousWordStart :: String -> Cursor -> [Int] -> Cursor
iterateToPreviousWordStart "" cursor' _ = cursor'
iterateToPreviousWordStart text cursor' linesSizes'
  | length text == 1 = Cursor (x cursor') (y cursor' - 1) -- before the last line
  | notElem (last text) "\r\n\t " && (last (init text) `elem` "\r\n\t ") = Cursor (x cursor') (y cursor' - 1) -- the start of the word
  | last text `elem` "\r\n" && (last (init text) `elem` "\r\n") = Cursor (x cursor' - 1) (linesSizes' !! (x cursor' - 1)) -- jump to line above
  | last text `elem` "\r\n" = iterateToPreviousWordStart (init text) (Cursor (x cursor' - 1) (linesSizes' !! (x cursor' - 1))) linesSizes' --
  | otherwise = iterateToPreviousWordStart (init text) (Cursor (x cursor') (y cursor' - 1)) linesSizes'

getRemainingInput :: String -> IO String
getRemainingInput lastChar = do
  char <- getChar
  if (char `elem` "hjkl$wbx") || not (isDigit char) && not (isDigit (head lastChar))
    then
      return [char]
    else do
      rest <- getRemainingInput [char]
      return (char : rest)
