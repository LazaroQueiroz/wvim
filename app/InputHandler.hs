module InputHandler where

import Editor.NormalMode
import Editor.InsertMode
import Editor.CommandMode
import Editor.EditorState
import Editor.Cursor

handleKeyPress :: EditorState -> [Char] -> IO EditorState

-- handleKeyPress state "\ESC" =
--   state { mode = Normal }
handleKeyPress state "\ESC[A" = do
  let (_, _, _, _, _, lineSizes) = (extendedPieceTable state)
  return state { cursor = (updateCursor 'k' (cursor state) lineSizes)}
handleKeyPress state "\ESC[B" = do
  let (_, _, _, _, _, lineSizes) = (extendedPieceTable state)
  return state { cursor = (updateCursor 'j' (cursor state) lineSizes)}
handleKeyPress state "\ESC[C" = do
  let (_, _, _, _, _, lineSizes) = (extendedPieceTable state)
  return state { cursor = (updateCursor 'l' (cursor state) lineSizes)}
handleKeyPress state "\ESC[D" = do
  let (_, _, _, _, _, lineSizes) = (extendedPieceTable state)
  return state { cursor = (updateCursor 'h' (cursor state) lineSizes)}
handleKeyPress state inputChar = case mode state of
  Normal -> do
    newState <- (handleNormalMode state inputChar)
    return newState
  Insert -> do
    newState <- (handleInsertMode state inputChar)
    return newState
  Command -> do
    newState <- (handleCommandMode state inputChar)
    return newState
  

