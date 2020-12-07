data BMI =
  VSUnderweight | SUnderweight | Underweight | Normal | Overweight | MObese | SObese | VSObese
  deriving (Enum, Show, Read)


bmiCalculator :: (Float, Float) -> BMI
bmiCalculator (w, h)
  | n < 15 = toEnum(0)
  | n < 16 = toEnum(1)
  | n < 18.5 = toEnum(2)
  | n < 25 = toEnum(3)
  | n < 30 = toEnum(4)
  | n < 35 = toEnum(5)
  | n < 40 = toEnum(6)
  | otherwise = toEnum(7)
  where n = w / (h ^ 2)
