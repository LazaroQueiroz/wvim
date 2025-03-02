module Utils where

nth :: Int -> [Int] -> Int
nth _ [] = 0
nth n seq' = seq' !! (n - 1)

count :: Char -> [Char] -> Int -> Int
count _ "" acc = acc
count targetChar (h : t) acc =
  count targetChar t (if h == targetChar then acc + 1 else acc)

replace :: String -> Char -> Char -> String -> String
replace "" _ _ acc = acc
replace (h : t) targetChar changeChar acc =
  replace t targetChar changeChar (acc ++ [if h == targetChar then changeChar else h])

isInsideClosedInterval :: Int -> Int -> Int -> Bool
isInsideClosedInterval number lowerBound upperBound = lowerBound <= number && number <= upperBound
