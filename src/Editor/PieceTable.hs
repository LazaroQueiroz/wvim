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

type PieceTable = ([Piece], Buffer, Buffer)

-- Cria uma Piece Table vazia
createPieceTable :: String -> PieceTable
createPieceTable originalText = 
    ([Piece Original 0 (length originalText)], originalText, "")


-- Insere texto na Piece Table
insertText :: String -> Int -> PieceTable -> PieceTable
insertText newText startIndex (pieces, originalBuffer, addBuffer) = 
    let newAddBuffer = addBuffer ++ newText
        newPiece = Piece Add (length addBuffer) (length newText)
        (before, after) = splitPieceCollection startIndex pieces
    in (before ++ [newPiece] ++ after, originalBuffer, newAddBuffer)

deleteText :: Int -> Int -> PieceTable -> PieceTable
deleteText startIndex length (pieces, originalBuffer, addBuffer) = 
    let (before1, after1) = splitPieceCollection startIndex pieces     
        (before2, after2) = splitPieceCollection length after1
    in (before1 ++ after2, originalBuffer, addBuffer)


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
pieceTableToString (pieces, originalBuffer, addBuffer) =
    foldl (\acc (Piece buf start len) ->
                acc ++ case buf of
                    Original -> take len (drop start originalBuffer)
                    Add      -> take len (drop start addBuffer)
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
  




  
