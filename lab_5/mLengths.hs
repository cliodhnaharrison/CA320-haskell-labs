-- Returns list of number of days in each month in that year
mLengths :: Int -> [Int]
mLengths y = if (leap y)
              then [31,29,31,30,31,30,31,31,30,31,30,31]
              else [31,28,31,30,31,30,31,31,30,31,30,31]


-- Checks if given year is a leap year
leap :: Int -> Bool
leap y
  | y `mod` 4 == 0 && y `mod` 100 == 0 && y `mod` 400 == 0 = True
  | y `mod` 4 == 0 && y `mod` 100 == 0 && y `mod` 400 /= 0 = False
  | y `mod` 4 == 0 = True
  | y == y = False
