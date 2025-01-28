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
  return state { cursor = (updateCursor 'k' (cursor state))}
handleKeyPress state "\ESC[B" = do
  return state { cursor = (updateCursor 'j' (cursor state))}
handleKeyPress state "\ESC[C" = do
  return state { cursor = (updateCursor 'l' (cursor state))}
handleKeyPress state "\ESC[D" = do
  return state { cursor = (updateCursor 'h' (cursor state))}
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
  

