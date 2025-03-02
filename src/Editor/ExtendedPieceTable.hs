module Editor.ExtendedPieceTable where

import Editor.Cursor
import Utils
import Prelude

data BufferType = Original | Add deriving (Show, Eq)

type Buffer = String

data Piece = Piece
  { bufferType :: BufferType,
    startIndex :: Int,
    pieceLength :: Int
  }
  deriving (Show)

type ExtendedPieceTable = ([Piece], Buffer, Buffer, Buffer, Int, [Int])

-- Cria uma Piece Table vazia
createExtendedPieceTable :: String -> ExtendedPieceTable
createExtendedPieceTable originalText =
  let linesSizes = getLinesSizes originalText 0 []
      firstPiece = [Piece Original 0 (length originalText)]
   in (firstPiece, originalText, "", "", 0, linesSizes)

-- Insere texto na Piece Table
insertText :: ExtendedPieceTable -> ExtendedPieceTable
insertText (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes)
  | null insertBuffer = (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes)
  | otherwise =
      let newAddBuffer = addBuffer ++ insertBuffer
          newPiece = Piece Add (length addBuffer) (length insertBuffer)
          (before, after) = splitPieceCollection insertStartIndex pieces
       in (before ++ [newPiece] ++ after, originalBuffer, newAddBuffer, "", insertStartIndex, linesSizes)

-- Deleta texto na Piece Table
deleteText :: Int -> Int -> ExtendedPieceTable -> ExtendedPieceTable
deleteText startDeleteIndex length' (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) =
  let (piecesBeforeDeletion, piecesAfterDeletionStart) = splitPieceCollection startDeleteIndex pieces
      (_, piecesAfterDeletion) = splitPieceCollection length' piecesAfterDeletionStart
      newPiecesCollection = piecesBeforeDeletion ++ piecesAfterDeletion
   in (newPiecesCollection, originalBuffer, addBuffer, insertBuffer, insertStartIndex - length', linesSizes)

splitPieceCollection :: Int -> [Piece] -> ([Piece], [Piece])
splitPieceCollection splitIndex =
  go splitIndex []
  where
    go _ acc [] = (reverse acc, [])
    go 0 acc rest = (reverse acc, rest)
    go n acc (currentPiece : remainingPieces)
      | n < pieceLength currentPiece =
          let (before, after) = splitPiece n currentPiece
           in (reverse (before ++ acc), after ++ remainingPieces)
      | otherwise =
          go (n - pieceLength currentPiece) (currentPiece : acc) remainingPieces

splitPiece :: Int -> Piece -> ([Piece], [Piece])
splitPiece splitIndex (Piece bufType pieceStartIndex len)
  | splitIndex <= 0 = ([], [Piece bufType pieceStartIndex len])
  | splitIndex >= len = ([Piece bufType pieceStartIndex len], [])
  | otherwise =
      ( [Piece bufType pieceStartIndex splitIndex],
        [Piece bufType (pieceStartIndex + splitIndex) (len - splitIndex)]
      )

extendedPieceTableToString :: ExtendedPieceTable -> String
extendedPieceTableToString (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, _) =
  foldl
    ( \acc (Piece bufType pieceStartIndex len) ->
        let pieceString = case bufType of
              Original -> take len (drop pieceStartIndex originalBuffer)
              Add -> take len (drop pieceStartIndex addBuffer)
            upperBound = length acc + len
            lowerBound = length acc
         in if isInsidePieceInterval insertStartIndex lowerBound upperBound
              then
                let pieceStringBeforeInsert = take (insertStartIndex - lowerBound) pieceString
                    pieceStringAfterInsert = drop (insertStartIndex - lowerBound) pieceString
                    newPieceString = pieceStringBeforeInsert ++ insertBuffer ++ pieceStringAfterInsert
                 in acc ++ newPieceString
              else acc ++ pieceString
    )
    ""
    pieces

isInsidePieceInterval :: Int -> Int -> Int -> Bool
isInsidePieceInterval = isInsideClosedInterval

extendedPieceTableToLineArray :: ExtendedPieceTable -> [String]
extendedPieceTableToLineArray extendedPieceTable = splitLines (extendedPieceTableToString extendedPieceTable)

splitLines :: String -> [String]
splitLines [] = []
splitLines text =
  let (pre, suf) = break isLineTerminator text
   in pre : case suf of
        ('\r' : '\n' : rest) -> splitLines rest
        ('\n' : rest) -> splitLines rest
        ('\r' : rest) -> splitLines rest
        _ -> []

isLineTerminator :: Char -> Bool
isLineTerminator char = (char == '\n') || (char == '\r')

getLinesSizes :: String -> Int -> [Int] -> [Int]
getLinesSizes [] lineSize acc = reverse (lineSize : acc)
getLinesSizes ('\n' : t) lineSize acc = getLinesSizes t 0 (lineSize : acc)
getLinesSizes (_ : t) lineSize acc = getLinesSizes t (lineSize + 1) acc

cursorXYToStringIndex :: Cursor -> [Int] -> Int -> Int -> Int
cursorXYToStringIndex (Cursor x' y') linesSizes acc lineIndex
  | lineIndex == x' = acc + y'
  | otherwise = case linesSizes of
      [] -> acc
      (h : t) -> cursorXYToStringIndex (Cursor x' y') t (acc + h) (lineIndex + 1)

updateLinesSizes :: [Char] -> Cursor -> [Int] -> [Int]
updateLinesSizes "\n" (Cursor x' y') linesSizes =
  let (linesSizesBeforeCursor, linesSizesFromCursor) = splitAt x' linesSizes
      cursorLineSize = head linesSizesFromCursor
      linesSizesAfterCursor =
        if null linesSizesFromCursor then [] else tail linesSizesFromCursor
      newLineSizeBeforeSplit = y'
      newLineSizeAfterSplit = cursorLineSize - y'
   in linesSizesBeforeCursor ++ [newLineSizeBeforeSplit] ++ [newLineSizeAfterSplit] ++ linesSizesAfterCursor
updateLinesSizes "\DEL" (Cursor x' y') linesSizes =
  if (x' == 0) && (y' == 0)
    then linesSizes
    else
      let (linesSizesBeforeCursor, linesSizesFromCursor) = splitAt x' linesSizes
          cursorLineSize = head linesSizesFromCursor
          previousToCursorLineSize = last linesSizesBeforeCursor
          newLineSize = cursorLineSize + previousToCursorLineSize
          linesSizesBeforeLineJoin = init linesSizesBeforeCursor
          linesSizesAfterCursor = tail linesSizesFromCursor
       in if y' == 0
            then linesSizesBeforeLineJoin ++ [newLineSize] ++ linesSizesAfterCursor
            else linesSizesBeforeCursor ++ [cursorLineSize - 1] ++ linesSizesAfterCursor
updateLinesSizes _ (Cursor x' _) linesSizes =
  let (linesSizesBeforeCursor, linesSizesFromCursor) = splitAt x' linesSizes
      cursorLineSize = head linesSizesFromCursor
      linesSizesAfterCursor =
        if null linesSizesFromCursor then [] else tail linesSizesFromCursor
   in (linesSizesBeforeCursor ++ [cursorLineSize + 1] ++ linesSizesAfterCursor)
