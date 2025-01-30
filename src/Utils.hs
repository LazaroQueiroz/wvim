module Utils where

nth :: Int -> [Int] -> Int
nth _ [] = 0
nth idx seq' = seq' !! max 0 (max 0 (idx - 1))

count :: Char -> [Char] -> Int -> Int
count _ "" acc = acc
count targetChar text acc =
  if head text == targetChar
    then count targetChar (tail text) (acc + 1)
    else count targetChar (tail text) acc

replace :: String -> Char -> Char -> String -> String
replace "" _ _ acc = acc
replace text targetChar changeChar acc =
  if head text == targetChar
    then replace (tail text) targetChar changeChar (acc ++ [changeChar])
    else replace (tail text) targetChar changeChar (acc ++ [head text])

isInsideClosedInterval :: Int -> Int -> Int -> Bool
isInsideClosedInterval number lowerBound upperBound = lowerBound <= number && number >= upperBound
