module Editor.VisualMode where

import Editor.Cursor (Cursor (Cursor), updateCursor, updateCursorPosition)
import Editor.EditorState
import Editor.ExtendedPieceTable
  ( cursorXYToStringIndex,
    deleteText,
    insertText,
    updateLinesSizes,
  )
import Utils

-- handleVisualMode :: EditorState -> [Char] -> IO EditorState
