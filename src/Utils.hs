module Utils where

nth :: Int -> [Int] -> Int
nth _ [] = 0
nth n seq' = seq' !! (n - 1)

count :: Char -> [Char] -> Int -> Int
count _ "" acc = acc
count targetChar (x : xs) acc =
  count targetChar xs (if x == targetChar then acc + 1 else acc)

replace :: String -> Char -> Char -> String -> String
replace "" _ _ acc = acc
replace (x : xs) targetChar changeChar acc =
  replace xs targetChar changeChar (acc ++ [if x == targetChar then changeChar else x])

isInsideClosedInterval :: Int -> Int -> Int -> Bool
isInsideClosedInterval number lowerBound upperBound = lowerBound <= number && number <= upperBound
