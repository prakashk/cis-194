-- CIS 194 -- Week 01

-- Exercise 01

-- Return the list of digits of the input integer
toDigits :: Integer -> [Integer]
toDigits n
  | n == 0 = [0]
  | n < 0  = []
  | n < 10 = [n]
  | otherwise = (toDigits $ n `div` 10) ++ (toDigits $ n `rem` 10)

-- Return the list of digits of the input integer in reverse order
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n == 0 = [0]
  | n < 0  = []
  | n < 10 = [n]
  | otherwise = (toDigitsRev $ n `rem` 10) ++ (toDigitsRev $ n `div` 10)

-- Exercise 02

-- Double every other digit, starting from the last one
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ds = doubleLastIfTrue ds False
  where
    doubleLastIfTrue [] _    = []
    doubleLastIfTrue xs True = (doubleLastIfTrue (init xs) False) ++ [2 * (last xs)]
    doubleLastIfTrue xs _    = (doubleLastIfTrue (init xs) True)  ++ [last xs]

-- Exercise 03

-- sum all digits of the input list of integers
sumDigits :: [Integer] -> Integer
sumDigits = sum . map sumNumDigits
  where
    sumNumDigits = sum . toDigits

-- Exercise 04

-- validate the input integer as a valid credit card number
validate :: Integer -> Bool
validate = isDivByTen . sumDigits . doubleEveryOther . toDigits
  where
    isDivByTen n = (n `rem` 10) == 0
