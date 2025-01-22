module Editor.NormalMode where

import System.IO
import System.Console.ANSI
import Editor.EditorState
import Editor.Cursor

handleNormalMode :: EditorState -> Char -> EditorState 
handleNormalMode state inputChar =
  updateEditorStateCursor state inputChar



