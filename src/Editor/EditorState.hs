module Editor.EditorState where

import Editor.Cursor
import Editor.ExtendedPieceTable
import Editor.StatusBar
import Editor.Viewport

data Mode = Normal | Insert | Command | Replace | Visual | Substitution | Closed deriving (Eq)

data FileStatus = Saved | NotSaved deriving (Eq)

data EditorState = EditorState
  { mode :: Mode,
    extendedPieceTable :: ExtendedPieceTable,
    cursor :: Cursor,
    viewport :: Viewport,
    fileStatus :: FileStatus,
    filename :: String,
    statusBar :: StatusBar,
    commandBuffer :: String, 
    undoStack :: [EditorState],
    redoStack :: [EditorState],
    visualModeStartIndex :: Int,
    copyBuffer :: String,
    searchBuffer :: String
  }

-- Creates an EditorState with default values.
defaultEditorState :: Int -> Int -> String -> EditorState
defaultEditorState rows' columns' filename' = EditorState Normal (createExtendedPieceTable "") (Cursor 0 0) (defaultViewport rows' columns') Saved filename' (StatusBar NoException "") "" [] [] 0 "" ""

-- Creates an EditorState from a file with content.
editorStateFromFile :: String -> Int -> Int -> String -> EditorState
editorStateFromFile file rows' columns' filename' = EditorState Normal (createExtendedPieceTable file) (Cursor 0 0) (defaultViewport rows' columns') Saved filename' (StatusBar NoException "") "" [] [] 0 "" ""

updateEditorStateCursor :: EditorState -> [Char] -> EditorState
updateEditorStateCursor state direction
  | direction `elem` ["h", "j", "k", "l"] =
      let (_, _, _, _, _, lineSizes) = extendedPieceTable state
          isInsertMode = mode state == Insert
          newCursor = updateCursor (head direction) (cursor state) lineSizes isInsertMode
       in state {cursor = newCursor}
  | otherwise = state

updateEditorStateViewport :: EditorState -> EditorState
updateEditorStateViewport currentState =
  let currentViewport = viewport currentState 
      Cursor x' y' = cursor currentState
      partiallyFixedViewport = updateViewport currentViewport (x', y')
      newViewport = updateViewport partiallyFixedViewport (x', y') 
  in currentState {viewport = newViewport}

addCurrentStateToUndoStack :: EditorState -> [EditorState] -> [EditorState]
addCurrentStateToUndoStack currentState undoStack' = undoStack' ++ [currentState {undoStack = [], redoStack = []}]

addCurrentStateToRedoStack :: EditorState -> [EditorState] -> [EditorState]
addCurrentStateToRedoStack currentState redoStack' = [currentState {undoStack = [], redoStack = []}] ++ redoStack'


