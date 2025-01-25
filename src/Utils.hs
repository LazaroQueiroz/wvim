module Utils where


nth :: Int -> [Int] -> Int
nth _ [] = 0
nth idx seq = (head (drop (max 0 (idx - 1)) seq))


count :: Char -> [Char] -> Int -> Int
count _ "" acc = acc
count targetChar text acc = 
  if (head text) == targetChar then (count targetChar (tail text) (acc + 1))
  else (count targetChar (tail text) acc)
  
