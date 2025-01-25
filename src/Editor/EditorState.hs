module Editor.EditorState where

import System.Console.ANSI
import Editor.ExtendedPieceTable
import Editor.Cursor
import Editor.Viewport

data Mode = Normal | Insert | Command | Closed


data EditorState = EditorState {
    mode :: Mode,
    extendedPieceTable :: ExtendedPieceTable,
    cursor :: Cursor,
    viewPort :: Viewport
}

defaultEditorState :: Int -> Int -> EditorState
defaultEditorState width height = (EditorState Normal (createExtendedPieceTable "texto original\nteste para quebra de linhas\nLázaro Queiroz") (Cursor 0 0) (defaultViewport width height))


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


  

