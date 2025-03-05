module Utils where

import Data.Char (isDigit)

-- Returns the nth element from a list (1-based index).
nth :: Int -> [Int] -> Int
nth _ [] = 0
nth n seq' = seq' !! (n - 1)

-- Counts the occurrences of a target character in a string.
count :: Char -> [Char] -> Int -> Int
count _ "" acc = acc
count targetChar (h : t) acc =
  count targetChar t (if h == targetChar then acc + 1 else acc)

-- Replaces all occurrences of a target character with another character in a string.
replace :: String -> Char -> Char -> String -> String
replace "" _ _ acc = acc
replace (h : t) targetChar changeChar acc =
  replace t targetChar changeChar (acc ++ [if h == targetChar then changeChar else h])

-- Checks if a number is inside a closed interval [lowerBound, upperBound].
isInsideClosedInterval :: Int -> Int -> Int -> Bool
isInsideClosedInterval number lowerBound upperBound = lowerBound <= number && number <= upperBound

isNumber :: Char -> Bool
isNumber c = c >= '0' && c <= '9'


getLeadingNumberAndRest :: String -> (Int, String)
getLeadingNumberAndRest s = 
  let (numStr, rest) = span isDigit s
      number = if null numStr then 1 else read numStr
  in (number, rest)

safeInit :: [Int] -> [Int]
safeInit [] = []
safeInit seq' = init seq'

safeLast :: [Int] -> Int
safeLast [] = 0
safeLast seq' = last seq'


