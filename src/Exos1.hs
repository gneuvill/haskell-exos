module Exos1  where

toDigits :: Integer -> [Integer]
toDigits d | d <= 0 = []
toDigits d = map charToInt $ show d
  where
    charToInt = read . (:[])

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x * 2]
doubleEveryOther [x, y] = [x * 2, y]
doubleEveryOther (x:xs) = x : doubleEveryOther xs
