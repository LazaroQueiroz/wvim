module Editor.VisualMode where

import Editor.Cursor (Cursor (Cursor), updateCursor, updateCursorPosition)
import Editor.EditorState
import Editor.ExtendedPieceTable (
  deleteText,
  insertText,
  updateLinesSizes, cursorXYToStringIndex,
 )
import Utils

--handleVisualMode :: EditorState -> [Char] -> IO EditorState