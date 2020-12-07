data Day_Names =
  Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday
  deriving (Eq, Enum, Show)

data Month_Names =
  Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec
  deriving (Eq, Enum, Show, Read)

type Day = Int
type Month = Month_Names
type Year = Int

-- Checks if given year is a leap year
leap :: Int -> Bool
leap y
  | y `mod` 4 == 0 && y `mod` 100 == 0 && y `mod` 400 == 0 = True
  | y `mod` 4 == 0 && y `mod` 100 == 0 && y `mod` 400 /= 0 = False
  | y `mod` 4 == 0 = True
  | y == y = False


-- Returns list of number of days in each month in that year
mLengths :: Int -> [Int]
mLengths y = if (leap y)
              then [31,29,31,30,31,30,31,31,30,31,30,31]
              else [31,28,31,30,31,30,31,31,30,31,30,31]

-- Returns number of days in previous years back to 1753
previousYears :: Year -> Int
previousYears y
  | y == 1753 = 0
  | y == 1754 = 365
  | leap (y - 1) = 366 + previousYears(y - 1)
  | not (leap (y - 1)) = 365 + previousYears(y - 1)

daysThisYear :: Month -> Year -> Int
daysThisYear m y = sum (take (fromEnum m) (mLengths y))

numDays :: (Day, Month, Year) -> Int
numDays (d, m, y) = (previousYears y) + (daysThisYear m y) + (d - 1)

dayOfWeek :: (Day, Month, Year) -> Day_Names
dayOfWeek (d, m, y) = toEnum (numDays (d, m, y) `mod` 7)
