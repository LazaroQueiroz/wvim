module Editor.EditorState where

import System.Console.ANSI
import Editor.ExtendedPieceTable
import Editor.Cursor
import Editor.Viewport

data Mode = Normal | Insert | Command | Closed deriving (Eq)


data EditorState = EditorState {
    mode :: Mode,
    extendedPieceTable :: ExtendedPieceTable,
    cursor :: Cursor,
    viewPort :: Viewport,
    filename :: String
}

defaultEditorState :: Int -> Int -> String -> EditorState
defaultEditorState width height filename = (EditorState Normal (createExtendedPieceTable "texto original\nteste para quebra de linhas\nLázaro Queiroz") (Cursor 0 0) (defaultViewport width height) filename)


editorStateFromFile :: String -> Int -> Int -> String -> EditorState
editorStateFromFile file width height filename = (EditorState Normal (createExtendedPieceTable file) (Cursor 0 0) (defaultViewport width height) filename)


updateEditorStateCursor :: EditorState -> [Char] -> EditorState
updateEditorStateCursor state "h" = 
  let newCursor = updateCursor 'h' (cursor state)
  in state { cursor = newCursor }
updateEditorStateCursor state "l" = 
  let newCursor = updateCursor 'l' (cursor state)
  in state { cursor = newCursor }
updateEditorStateCursor state "k" = 
  let newCursor = updateCursor 'k' (cursor state)
  in state { cursor = newCursor }
updateEditorStateCursor state "j" = 
  let newCursor = updateCursor 'j' (cursor state)
  in state { cursor = newCursor }


  

