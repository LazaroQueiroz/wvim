module Editor.PieceTable where 


import Data.Typeable
import Prelude

data BufferType = Original | Add deriving (Show, Eq)

type Buffer = String

data Piece = Piece {
  bufferType :: BufferType,
  startIndex :: Int,
  pieceLength :: Int
} deriving Show

type PieceTable = ([Piece], Buffer, Buffer, (Buffer, Int), [Int])

-- Cria uma Piece Table vazia
createPieceTable :: String -> PieceTable
createPieceTable originalText = 
    ([Piece Original 0 (length originalText)], originalText, "", ("", 0), (getLineSizesArray originalText 0 []))

-- Insere texto na Piece Table
insertText :: String -> Int -> PieceTable -> PieceTable
insertText iBuffer startIndex (pieces, originalBuffer, addBuffer, insertBuffer, sizesSeq) = 
    let newAddBuffer = addBuffer ++ iBuffer
        newPiece = Piece Add (length addBuffer) (length iBuffer)
        (before, after) = splitPieceCollection startIndex pieces
        (_, startPos) = insertBuffer
    in (before ++ [newPiece] ++ after, originalBuffer, newAddBuffer, ("", startPos), sizesSeq)

deleteText :: Int -> Int -> Int -> PieceTable -> PieceTable
deleteText startIndex length linePos (pieces, originalBuffer, addBuffer, insertBuffer, sizesSeq) = 
    let (before1, after1) = splitPieceCollection startIndex pieces     
        (before2, after2) = splitPieceCollection length after1
        newSizesSeq = (changeLineSize (- length) linePos sizesSeq)
    in (before1 ++ after2, originalBuffer, addBuffer, insertBuffer, newSizesSeq)

splitPieceCollection :: Int -> [Piece] -> ([Piece], [Piece])
splitPieceCollection startIndex pieces = 
    go startIndex [] pieces  
  where 
    go 0 acc rest = (reverse acc, rest)
    go n acc (p:ps)
        | n < pieceLength p = 
          let (before, after) = splitPiece n p
          in (reverse (before ++ acc), (after ++ ps))
        | otherwise =  
          go (n - pieceLength p) (p : acc) ps
    go _ acc [] = (reverse acc, [])

splitPiece :: Int -> Piece -> ([Piece], [Piece])
splitPiece pos (Piece bufferType startIndex length)
    | pos <= 0 = ([], [Piece bufferType startIndex length])
    | pos >= length = ([Piece bufferType startIndex length], [])
    | otherwise =
      ([(Piece bufferType startIndex pos)]
      ,[(Piece bufferType (startIndex + pos) (length - pos))])

pieceTableToString :: PieceTable -> String
pieceTableToString (pieces, originalBuffer, addBuffer, insertBuffer, sizesSeq) =
    foldl (\acc (Piece buf start len) ->
                let (iBuff, startPos) = insertBuffer
                    stringPiece = case buf of
                        Original -> take len (drop start originalBuffer)
                        Add      -> take len (drop start addBuffer)
                    diffStartPos = ((length acc) + len)
                in 
                  if ((diffStartPos) >= startPos) && (startPos >= (length acc)) then 
                    acc ++ (take (startPos - (length acc)) stringPiece) ++ iBuff ++ (drop (startPos - (length acc)) stringPiece)
                  else acc ++ stringPiece
                  -- if (startPos == (length acc)) then
                  --   acc ++ iBuff ++ case buf of
                  --       Original -> take len (drop start originalBuffer)
                  --       Add      -> take len (drop start addBuffer)
                  -- else 
                  --   acc ++ case buf of
                  --       Original -> take len (drop start originalBuffer)
                  --       Add      -> take len (drop start addBuffer)
           ) "" pieces 

pieceTableToLineArray :: PieceTable -> [String]
pieceTableToLineArray pieceTable = (splitLines (pieceTableToString pieceTable))

splitLines :: String -> [String]
splitLines [] = []
splitLines text = 
  let (pre, suf) = break isLineTerminator text
  in pre : case suf of
      ('\n':rest) -> splitLines rest
      ('\r':rest) -> splitLines rest
      _           -> []

isLineTerminator :: Char -> Bool
isLineTerminator char = (char == '\n') || (char == '\r')
  
getLineSizesArray :: String -> Int -> [Int] -> [Int]
getLineSizesArray "" lineSize sizesSeq = (reverse (lineSize : sizesSeq))
getLineSizesArray text lineSize sizesSeq =
  if (head text) == '\n' then (getLineSizesArray (tail text) 0 ((lineSize + 1) : sizesSeq))
  else (getLineSizesArray (tail text) (lineSize + 1) sizesSeq)
 
changeLineSize :: Int -> Int -> [Int] -> [Int]
changeLineSize charSizeChange lineNumber sizesSeq = 
  let (before, after) = (splitAt lineNumber sizesSeq)
      lineSize = (head after)
  in (before ++ [lineSize + charSizeChange] ++ (tail after))

-- countCharPosition text curPos curRow curColumn targetRow targetColumn =
--   if curRow == targetRow && curColumn == targetColumn then curPos 
--   else if (head text) == '\n' then (countCharPosition (tail text) (curPos + 1) (curRow + 1) 0 targetRow targetColumn)
--   else (countCharPosition (tail text) (curPos + 1) curRow (curColumn + 1) targetRow targetColumn)
  

-- [3 4 14 22 99 34] <-
-- [3 4 14] [22] [99 34]
-- [3 4 14]++[25]++[99 34] = [3 4 14 25 99 34]
--
-- ------------------
-- "lazaro\nrafael"
-- posPiece = 8
-- posGrid = (1 1)
--
-- --
-- 0 0 1 2 3 4 5 6
-- 0 l a z 
-- 1 r a f a e l
-- 2 c a r l o s
-- 4
--
  
