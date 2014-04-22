-- CIS 194 -- Week 01

-- Exercise 01

toDigits :: Integer -> [Integer]
toDigits n
  | n == 0 = [0]
  | n < 0  = []
  | n < 10 = [n]
  | otherwise = (toDigits $ n `div` 10) ++ (toDigits $ n `rem` 10)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n == 0 = [0]
  | n < 0  = []
  | n < 10 = [n]
  | otherwise = (toDigitsRev $ n `rem` 10) ++ (toDigitsRev $ n `div` 10)

-- Exercise 02

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ds = doubleLastIfTrue ds False
  where
    doubleLastIfTrue [] _    = []
    doubleLastIfTrue xs True = (doubleLastIfTrue (init xs) False) ++ [2 * (last xs)]
    doubleLastIfTrue xs _    = (doubleLastIfTrue (init xs) True)  ++ [last xs]

-- Exercise 03

sumDigits :: [Integer] -> Integer
sumDigits = sum . map sumNumDigits
  where
    sumNumDigits = sum . toDigits

-- Exercise 04

validate :: Integer -> Bool
validate = isDivByTen . sumDigits . doubleEveryOther . toDigits
  where
    isDivByTen n = (n `rem` 10) == 0
