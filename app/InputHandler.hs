module InputHandler where

import Editor.NormalMode
import Editor.InsertMode
import Editor.CommandMode
import Editor.EditorState

handleKeyPress :: EditorState -> Char -> EditorState
handleKeyPress state inputChar = case mode state of
  Normal -> handleNormalMode state inputChar
  Insert -> handleInsertMode state inputChar
  Command -> handleCommandMode state inputChar
  

