module Editor.EditorState where

import System.Console.ANSI
import Editor.ExtendedPieceTable
import Editor.Cursor
import Editor.Viewport
import Editor.StatusBar

data Mode = Normal | Insert | Command | Closed deriving (Eq)
data FileStatus = Saved | NotSaved deriving (Eq)

data EditorState = EditorState {
    mode :: Mode,
    extendedPieceTable :: ExtendedPieceTable,
    cursor :: Cursor,
    viewPort :: Viewport,
    fileStatus :: FileStatus,
    filename :: String,
    statusBar :: StatusBar
}

defaultEditorState :: Int -> Int -> String -> EditorState
defaultEditorState width height filename = (EditorState Normal (createExtendedPieceTable "") (Cursor 0 0) (defaultViewport width height) Saved filename (StatusBar NoException ""))


editorStateFromFile :: String -> Int -> Int -> String -> EditorState
editorStateFromFile file width height filename = (EditorState Normal (createExtendedPieceTable file) (Cursor 0 0) (defaultViewport width height) Saved filename (StatusBar NoException ""))


updateEditorStateCursor :: EditorState -> [Char] -> EditorState
updateEditorStateCursor state "h" = 
  let (_, _, _, _, _, lineSizes) = (extendedPieceTable state)
      newCursor = updateCursor 'h' (cursor state) lineSizes
  in state { cursor = newCursor }
updateEditorStateCursor state "l" = 
  let (_, _, _, _, _, lineSizes) = (extendedPieceTable state)
      newCursor = updateCursor 'l' (cursor state) lineSizes
  in state { cursor = newCursor }
updateEditorStateCursor state "k" = 
  let (_, _, _, _, _, lineSizes) = (extendedPieceTable state)
      newCursor = updateCursor 'k' (cursor state) lineSizes
  in state { cursor = newCursor }
updateEditorStateCursor state "j" = 
  let (_, _, _, _, _, lineSizes) = (extendedPieceTable state)
      newCursor = updateCursor 'j' (cursor state) lineSizes
  in state { cursor = newCursor }
updateEditorStateCursor state inputString = state


  

