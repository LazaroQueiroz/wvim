module InputHandler where

import Editor.NormalMode
import Editor.InsertMode
import Editor.CommandMode
import Editor.EditorState
import Editor.Cursor

handleKeyPress :: EditorState -> [Char] -> EditorState
-- handleKeyPress state "\ESC" =
--   state { mode = Normal }
handleKeyPress state "\ESC[A" =
  state { cursor = (updateCursor 'k' (cursor state))}
handleKeyPress state "\ESC[B" =
  state { cursor = (updateCursor 'j' (cursor state))}
handleKeyPress state "\ESC[C" =
  state { cursor = (updateCursor 'l' (cursor state))}
handleKeyPress state "\ESC[D" =
  state { cursor = (updateCursor 'h' (cursor state))}
handleKeyPress state inputChar = case mode state of
  Normal -> handleNormalMode state inputChar
  Insert -> handleInsertMode state inputChar
  Command -> handleCommandMode state inputChar
  

