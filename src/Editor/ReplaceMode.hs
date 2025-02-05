module Editor.ReplaceMode where

import Editor.Cursor (Cursor (Cursor), updateCursor, updateCursorPosition)
import Editor.EditorState
import Editor.ExtendedPieceTable (
  deleteText,
  insertText,
  updateLinesSizes, cursorXYToStringIndex,
 )
import Utils

--handleReplaceMode :: EditorState -> [Char] -> IO EditorState