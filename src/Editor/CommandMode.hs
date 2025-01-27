module Editor.CommandMode where

import Editor.EditorState
import Editor.ExtendedPieceTable
import System.Directory (renameFile)
import System.IO (writeFile)

handleCommandMode :: EditorState -> [Char] -> IO ()
handleCommandMode state "w" = do
  writeFile ("." ++ (filename state) ++ ".swp") (extendedPieceTableToString (extendedPieceTable state))
  renameFile ("." ++ (filename state) ++ ".swp") (filename state)
   
