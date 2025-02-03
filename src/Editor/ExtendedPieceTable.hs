module Editor.ExtendedPieceTable where

import Editor.Cursor
import Utils
import Prelude

data BufferType = Original | Add deriving (Show, Eq)

type Buffer = String

data Piece = Piece
  { bufferType :: BufferType
  , startIndex :: Int
  , pieceLength :: Int
  }
  deriving (Show)

piecesCollToString :: [Piece] -> [String]
piecesCollToString = map pieceToString

pieceToString :: Piece -> String
pieceToString piece = "buf:" ++ show (head (show (bufferType piece))) ++ "|idx:" ++ show (startIndex piece) ++ "|len:" ++ show (pieceLength piece)

type ExtendedPieceTable = ([Piece], Buffer, Buffer, Buffer, Int, [Int])

-- Creates an empty Piece Table with an initial piece containing the original text.
createExtendedPieceTable :: String -> ExtendedPieceTable
createExtendedPieceTable originalText =
  let linesSizes = getLinesSizes originalText 0 []
      firstPiece = [Piece Original 0 (length originalText)]
   in (firstPiece, originalText, "", "", 0, linesSizes)

-- Inserts text into the Piece Table by adding new pieces based on the input buffer.
insertText :: ExtendedPieceTable -> ExtendedPieceTable
insertText (pieces, originalBuffer, addBuffer, "", insertStartIndex, linesSizes) = 
  (pieces, originalBuffer, addBuffer, "", insertStartIndex, linesSizes)
insertText (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) =
  let newAddBuffer = addBuffer ++ insertBuffer
      newPiece = Piece Add (length addBuffer) (length insertBuffer)
      (before, after) = splitPieceCollection insertStartIndex pieces
   in (before ++ [newPiece] ++ after, originalBuffer, newAddBuffer, "", insertStartIndex, linesSizes)

-- Deletes text in Piece Table by removing the corresponding range of pieces 
deleteText :: Int -> Int -> ExtendedPieceTable -> ExtendedPieceTable
deleteText startDeleteIndex length' (pieces, originalBuffer, addBuffer, insertBuffer, insertStartIndex, linesSizes) =
  let (piecesBeforeDeletion, piecesAfterDeletionStart) = splitPieceCollection (startDeleteIndex - 1) pieces
      (_, piecesAfterDeletion) = splitPieceCollection length' piecesAfterDeletionStart
      newPiecesCollection = piecesBeforeDeletion ++ piecesAfterDeletion
   in (newPiecesCollection, originalBuffer, addBuffer, insertBuffer, insertStartIndex - length', linesSizes)

-- Splits the Piece Collection at the specified index, returning two parts: one before the index and one after the index.
splitPieceCollection :: Int -> [Piece] -> ([Piece], [Piece])
splitPieceCollection splitIndex = go splitIndex []
 where
  go _ acc [] = (reverse acc, [])
  go 0 acc rest = (reverse acc, rest)
  go n acc (currentPiece : remainingPieces)
    | n < pieceLength currentPiece =
        let (before, after) = splitPiece n currentPiece
         in (reverse (before ++ acc), after ++ remainingPieces)
    | otherwise =
        go (n - pieceLength currentPiece) (currentPiece : acc) remainingPieces

-- Splits a single Piece at the specified index, returning two new pieces: one before the index and one after the index.
splitPiece :: Int -> Piece -> ([Piece], [Piece])
splitPiece splitIndex (Piece bufType pieceStartIndex len)
  | splitIndex <= 0 = ([], [Piece bufType pieceStartIndex len])
  | splitIndex >= len = ([Piece bufType pieceStartIndex len], [])
  | otherwise =
      ( [Piece bufType pieceStartIndex splitIndex]
      , [Piece bufType (pieceStartIndex + splitIndex) (len - splitIndex)]
      )

-- Idk
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
    where 
      isInsidePieceInterval = isInsideClosedInterval

-- Converts the extended piece table into a list of strings, representing lines of text.
extendedPieceTableToLineArray :: ExtendedPieceTable -> [String]
extendedPieceTableToLineArray extendedPieceTable = splitLines (extendedPieceTableToString extendedPieceTable)

-- Converts a Cursor's position (x, y) to the corresponding string index in the buffer
cursorXYToStringIndex :: Cursor -> [Int] -> Int -> Int -> Int
cursorXYToStringIndex (Cursor _ _) [] acc _ = acc
cursorXYToStringIndex (Cursor x' y') (h:t) acc lineIndex
  | lineIndex == x' = acc + y' + x'
  | otherwise = cursorXYToStringIndex (Cursor x' y') t (acc + h) (lineIndex + 1)

-- Splits a string into a list of lines, spliting them with line breaks '\r\n' and '\n'.
splitLines :: String -> [String]
splitLines [] = []
splitLines text =
  let (pre, suf) = break (`elem` "\r\n") text
   in pre : case suf of
        ('\r' : '\n' : rest) -> splitLines rest
        (_ : rest) -> splitLines rest
        _ -> []

-- Computes the sizes (lengths) of each line in a string, returning a list of integers.
getLinesSizes :: String -> Int -> [Int] -> [Int]
getLinesSizes [] lineSize acc = reverse (lineSize : acc)
getLinesSizes ('\n' : t) lineSize acc = getLinesSizes t 0 (lineSize : acc)
getLinesSizes (_ : t) lineSize acc = getLinesSizes t (lineSize + 1) acc

-- Updates the line sizes based on the user input, returning the new line sizes.
updateLinesSizes :: [Char] -> Cursor -> [Int] -> [Int]
updateLinesSizes inputChar (Cursor x' y') linesSizes
  | inputChar == "\n" = beforeCursor ++ [y', cursorLineSize - y'] ++ afterCursor 
  | inputChar == "\DEL" && y' == 0 = init beforeCursor ++ [cursorLineSize + last beforeCursor] ++ afterCursor
  | inputChar == "\DEL" = beforeCursor ++ [cursorLineSize - 1] ++ afterCursor
  | otherwise = beforeCursor ++ [cursorLineSize + 1] ++ afterCursor
  where
    (beforeCursor, fromCursor) = splitAt x' linesSizes
    cursorLineSize = head fromCursor
    afterCursor
      | null fromCursor = []
      | otherwise = tail fromCursor

-- Mais conciso porém menos informativo
--updateLinesSizes :: [Char] -> Cursor -> [Int] -> [Int]
--updateLinesSizes inputChar (Cursor x' y') linesSizes
--  | inputChar == "\n" =
--      let (linesSizesBeforeCursor, linesSizesFromCursor) = splitAt x' linesSizes
--          cursorLineSize = head linesSizesFromCursor
--          newLineSizeBeforeSplit = y'
--          newLineSizeAfterSplit = cursorLineSize - y'
--      in linesSizesBeforeCursor ++ [newLineSizeBeforeSplit] ++ [newLineSizeAfterSplit] ++ linesSizesAfterCursor
--  | inputChar == "\DEL", y' == 0 =
--      let (linesSizesBeforeCursor, linesSizesFromCursor) = splitAt x' linesSizes
--          cursorLineSize = head linesSizesFromCursor
--          previousToCursorLineSize = last linesSizesBeforeCursor
--          newLineSize = cursorLineSize + previousToCursorLineSize
--          linesSizesBeforeLineJoin = init linesSizesBeforeCursor
--          linesSizesAfterCursor = tail linesSizesFromCursor
--      in linesSizesBeforeLineJoin ++ [newLineSize] ++ linesSizesAfterCursor
--  | inputChar == "\DEL" =
--      let (linesSizesBeforeCursor, linesSizesFromCursor) = splitAt x' linesSizes
--          cursorLineSize = head linesSizesFromCursor
--      in linesSizesBeforeCursor ++ [cursorLineSize - 1] ++ linesSizesAfterCursor
--  | otherwise =
--      let (linesSizesBeforeCursor, linesSizesFromCursor) = splitAt x' linesSizes
--          cursorLineSize = head linesSizesFromCursor
--      in linesSizesBeforeCursor ++ [cursorLineSize + 1] ++ linesSizesAfterCursor
--    where
--      linesSizesAfterCursor
--        | null fromCursor = []
--        | otherwise = tail fromCursor