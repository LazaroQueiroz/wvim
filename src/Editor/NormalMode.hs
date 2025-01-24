module Editor.NormalMode where

import System.IO
import System.Console.ANSI
import Editor.EditorState
import Editor.Cursor

handleNormalMode :: EditorState -> [Char] -> EditorState 
handleNormalMode state "i" = 
  let newMode = Insert
  in state { mode = newMode }
handleNormalMode state "I" = 
  let newMode = Insert
  in state { mode = newMode }
handleNormalMode state ":" = 
  let newMode = Command
  in state { mode = newMode }
handleNormalMode state inputChar =
  updateEditorStateCursor state inputChar



