delOdds :: [Int] -> [Int]
delOdds [] = []
delOdds (x:xs)
    | x `mod` 2 /= 0 = delOdds xs
    | otherwise = x:(delOdds xs)
