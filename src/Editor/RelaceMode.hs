module Editor.ReplaceMode where

import Editor.Cursor (Cursor (Cursor), updateCursor, updateCursorPosition)
import Editor.EditorState
import Editor.ExtendedPieceTable
  ( cursorXYToStringIndex,
    deleteText,
    insertText,
    updateLinesSizes,
  )
import Utils

-- handleReplaceMode :: EditorState -> [Char] -> IO EditorState
