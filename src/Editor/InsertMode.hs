module Editor.InsertMode where

import System.IO
import System.Console.ANSI

import Editor.Cursor
import Editor.EditorState


handleInsertMode :: EditorState -> Char -> EditorState
handleInsertMode state inputChar = defaultEditorState 0 0

-- Creates a function to transform a single string buffer into a 
-- array of strings, where each string represents a line in the grid
splitLines :: String -> [String]
splitLines buffer = 
  let (line, rest) = break (== '\n') buffer
  in line : if null rest then [] else splitLines (tail rest)


insertCommandManager :: String -> Cursor -> Char -> (Cursor, String)
insertCommandManager buffer cursor char = (cursor, buffer)
  -- printBufferToGrid buffer
