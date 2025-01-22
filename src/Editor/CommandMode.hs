module Editor.CommandMode where

import Editor.EditorState

handleCommandMode :: EditorState -> Char -> EditorState
handleCommandMode state inputChar = defaultEditorState 0 0
