import System.IO
import System.Console.ANSI

-- Initial Cursor Position
data Cursor = Cursor { x :: Int, y :: Int } deriving Show 
-- data Grid = Grid { grid :: [[Char]] } deriving Show 

-- Move the cursor in the terminal
moveCursor :: Cursor -> IO () -- type declaration
moveCursor (Cursor x y) = setCursorPosition y x -- set cursor position comes from the library (Console.ANSI)

-- Update cursor position based on input
-- Pattern Matching: works like a switch case based on the inputs to the function.
updateCursor :: Char -> Cursor -> Cursor
updateCursor 'k' (Cursor x y) = Cursor x (max 0 (y - 1))  -- Move up
updateCursor 'j' (Cursor x y) = Cursor x (y + 1)          -- Move down
updateCursor 'h' (Cursor x y) = Cursor (max 0 (x - 1)) y  -- Move left
updateCursor 'l' (Cursor x y) = Cursor (x + 1) y          -- Move right
updateCursor _ cursor = cursor                            -- No change


modeController :: Char -> Char -> Char -- current mode, typed key -> next mode
modeController _ '\27' = 'N' -- goes to normal mode when ESCAPE key is pressed.
modeController 'I' _ = 'I'
modeController 'C' _ = 'C'
modeController 'N' ':' = 'C'
modeController 'N' 'i' = 'I'
modeController 'N' 'I' = 'I'
modeController 'N' _ = 'N'


normalCommandManager :: String -> Cursor -> Char -> IO ()
normalCommandManager buffer cursor inputChar = do
  let newCursor = updateCursor inputChar cursor
  moveCursor newCursor
  hFlush stdout
  inputControlLoop buffer newCursor 'N' 


-- Creates a function to display the contents of the buffer in a grid
-- uses a mapping function (putStrLn to the buffer)
printBufferToGrid :: String -> IO ()
printBufferToGrid buffer = mapM_ putStrLn (splitLines buffer)


-- Creates a function to transform a single string buffer into a 
-- array of strings, where each string represents a line in the grid
splitLines :: String -> [String]
splitLines buffer = 
  let (line, rest) = break (== '\n') buffer
  in line : if null rest then [] else splitLines (tail rest)

insertCommandManager :: String -> Cursor -> Char -> IO () 
insertCommandManager buffer cursor char = do
  hFlush stdout
  printBufferToGrid buffer
  inputControlLoop buffer cursor 'I'

-- Main loop for controlling the cursor
-- main loop: input (a) 
inputControlLoop :: String -> Cursor -> Char -> IO ()
inputControlLoop buffer cursor curMode = do
  -- moveCursor cursor
  hFlush stdout
  inputChar <- getChar
  let nextMode = modeController curMode inputChar
  case nextMode of
     'N' -> normalCommandManager buffer cursor inputChar
     'I' -> insertCommandManager buffer cursor inputChar
     _ -> print nextMode
     -- 'C' -> commandCommandManager inputChar
  -- inputControlLoop nextMode

main :: IO ()
main = do
  let buffer = "testing the line printing\nthere a break up there"
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clearScreen
  dados <- hGetTerminalSize stdout
  case dados of
    Just (width, height) -> do
      putStrLn $ "this is the width: " ++ show width ++ " and this is the height: " ++ show height
    Nothing -> putStrLn "ja era"
  putStrLn "Use h, j, k, l to move the cursor. Press 'q' to quit."
  normalCommandManager buffer (Cursor 0 0) ' ' -- TODO: remember to solve this problem, we must not use a blank space to start the program.
  -- inputControlLoop (Cursor 0 0) 'N'

