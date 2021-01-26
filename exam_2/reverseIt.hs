reverseIt :: [Int] -> [Int]
reverseIt [] = []
reverseIt (x:xs) = (reverseIt xs) ++ [x]
