module Editor.EditorState where

import Editor.Cursor
import Editor.ExtendedPieceTable
import Editor.StatusBar
import Editor.Viewport

data Mode = Normal | Insert | Command | Closed deriving (Eq)

data FileStatus = Saved | NotSaved deriving (Eq)

data EditorState = EditorState
  { mode :: Mode,
    extendedPieceTable :: ExtendedPieceTable,
    cursor :: Cursor,
    viewport :: Viewport,
    fileStatus :: FileStatus,
    filename :: String,
    statusBar :: StatusBar
  }

defaultEditorState :: Int -> Int -> String -> EditorState
defaultEditorState width height filename' = EditorState Normal (createExtendedPieceTable "") (Cursor 0 0) (defaultViewport width height) Saved filename' (StatusBar NoException "")

editorStateFromFile :: String -> Int -> Int -> String -> EditorState
editorStateFromFile file width height filename' = EditorState Normal (createExtendedPieceTable file) (Cursor 0 0) (defaultViewport width height) Saved filename' (StatusBar NoException "")

updateEditorStateCursor :: EditorState -> [Char] -> EditorState
updateEditorStateCursor state direction
  | direction `elem` ["h", "j", "k", "l"] =
      let (_, _, _, _, _, lineSizes) = extendedPieceTable state
          isInsertMode = mode state == Insert
          newCursor = updateCursor (head direction) (cursor state) lineSizes isInsertMode
       in state {cursor = newCursor}
  | otherwise = state
