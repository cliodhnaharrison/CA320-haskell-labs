-- Checks if given year is a leap year
leap :: Int -> Bool
leap y
  | y `mod` 4 == 0 && y `mod` 100 == 0 && y `mod` 400 == 0 = True
  | y `mod` 4 == 0 && y `mod` 100 == 0 && y `mod` 400 /= 0 = False
  | y `mod` 4 == 0 = True
  | y == y = False
