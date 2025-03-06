module Main where

import Control.Concurrent (threadDelay)
import Editor.EditorState
import Editor.Viewport
import InputHandler
import Renderer
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO
import System.Process (callCommand)

-- Read a character with a brief timeout for distinguishing ESC vs arrow keys
-- @return String that represents the character (composite or individual) received from the user.
getCharRaw :: IO String
getCharRaw = do
  char <- getChar
  if char == '\ESC'
    then do
      threadDelay 2000 -- Pequeno atraso para evitar leituras excessivas
      isCharInHandleBuffer <- hReady stdin
      if isCharInHandleBuffer
        then getChars [char] -- Captura sequência completa
        else return [char]
    else return [char]

-- Função auxiliar que captura uma sequência de caracteres
getChars :: [Char] -> IO String
getChars charArray = do
  isCharInHandleBuffer <- hReady stdin
  if isCharInHandleBuffer
    then do
      char <- getChar
      getChars (charArray ++ [char]) -- Funciona, agora vamos ter que resolver as coisas no inputHandler ;-;
    else return charArray

-- Sets the terminal configurations: disable input buffering and input echoing
setTerminalConfiguration :: IO ()
setTerminalConfiguration = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clearScreen

-- Sets the terminal configurations to disable input character buffering and input echoing (writing the input in the terminal as it is received) and start the main event loop.
-- TODO aplicar functors para simplificar os ifs
main :: IO ()
main = do
  args <- getArgs
  Just (rows, columns) <- getTerminalSize
  setTerminalConfiguration

  startingState <- case args of
    [] -> return (defaultEditorState rows columns "") -- Default editor state if no args
    [nameOfTheFile] -> do
      exists <- doesFileExist nameOfTheFile -- Check if file exists
      if exists
        then do
          file <- readFile nameOfTheFile -- Read the file content
          return (editorStateFromFile file rows columns nameOfTheFile) -- Create EditorState from file
        else do
          return (defaultEditorState rows columns nameOfTheFile) -- Return default state if file doesn't exist
    _ -> return (defaultEditorState rows columns "") -- Fallback for extra arguments, use default state
  eventLoop [startingState] 0

-- Mantain the main recursion loop running. Based on the current editor state, it renders this state, process the user input and then process the new state based on it.
-- @param editorState :: EditorState - current state of the editor.
-- eventLoop :: EditorState -> IO ()
-- eventLoop = unfoldM step
--   where
--     step editorState = do
--       renderState editorState
--       inputString <- getCharRaw
--       newState <- handleKeyPress editorState inputString
--       return $ if isRunning newState then Just (updateEditorStateViewport newState) else Nothing
eventLoop :: [EditorState] -> Int -> IO ()
eventLoop states currentIndex = do
  let currentState = (states !! currentIndex) 
      viewport' = viewport currentState
  renderState currentState
  inputChar <- getCharRaw

  if mode currentState == Normal then
    case inputChar of
      "[" -> do
        let newIndex = max 0 (currentIndex - 1)  -- Movendo para o estado à esquerda
        eventLoop states newIndex
      
      "]" -> do
        let newIndex = min (length states - 1) (currentIndex + 1)  -- Movendo para o estado à direita
        eventLoop states newIndex
      
      "{" -> do
        let newState = defaultEditorState (rows viewport') (columns viewport') ""
            newStates = states ++ [newState]
        let newIndex = min (length newStates - 1) (currentIndex + 1)  -- O novo estado é adicionado à lista
        eventLoop newStates newIndex
      
      _ -> handleOtherInput currentState inputChar states currentIndex
    else
      handleOtherInput currentState inputChar states currentIndex


handleOtherInput :: EditorState -> String -> [EditorState] -> Int -> IO ()
handleOtherInput currentState inputChar states currentIndex = do
  newState' <- handleKeyPress currentState inputChar
  let newStates = replaceAt currentIndex newState states
      newState = updateEditorStateViewport newState'

  if mode newState == Closed then do
    let (left, right) = splitAt currentIndex newStates
    let nextStateIndex = if null right then max 0 (currentIndex - 1) else 0

    if null right && nextStateIndex == 0 || length newStates == 1 then
      return ()
    else
      eventLoop (left ++ tail right) nextStateIndex

  else
    eventLoop newStates currentIndex


replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []  -- Caso base: lista vazia
replaceAt 0 newVal (x:xs) = newVal : xs  -- Substitui o primeiro elemento
replaceAt n newVal (x:xs)
  | n > 0     = x : replaceAt (n - 1) newVal xs  -- Recursão, decrementando n até atingir a posição
  | otherwise = xs  -- Caso em que n é negativo ou fora do alcance, retorna a lista sem alterações

unfoldM :: (a -> IO (Maybe a)) -> a -> IO ()
unfoldM f a = do
  result <- f a
  maybe (return ()) (unfoldM f) result

-- Verifies if the current editor state is a valid (or running) state. If this is the case, return True, otherwise, False.
-- @param editorState :: EditorState - current state of the editor.
isRunning :: EditorState -> Bool
isRunning (EditorState Closed _ _ _ _ _ _ _ _ _ _ _ _) = False
isRunning _ = True
