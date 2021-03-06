module Week1.Exos where

-- Credit cards number

validate :: Integer -> Bool
validate d = remainder d == 0
  where
    remainder =
      snd . divByTen . (sumDigits . doubleEveryOther . toDigits)
    divByTen = (flip quotRem) 10

toDigits :: Integer -> [Integer]
toDigits = myReverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev d = decompose $ quotRem d 10
  where
    decompose (q, r) | q == 0 = [r]
                     | otherwise = r : toDigitsRev q

-- toDigits d
--   | d <= 0 = []
--   | otherwise = [charToInt x | x <- show d]
--   where
--     charToInt = read . (:[])

-- toDigitsRev :: Integer -> [Integer]
-- toDigitsRev = myReverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = deo . myReverse
  where
    deo [] = []
    deo [x] = [x]
    deo [x, y] = [y * 2, x]
    deo (x:y:xs) = deo xs ++ [y * 2, x]

sumDigits :: [Integer] -> Integer
sumDigits ds = sumL $ concatL [toDigits d | d <- ds] []
  where
    concatL [] acc = acc
    concatL ([]:l) acc = concatL l acc
    concatL ((x:xs):ls) acc = concatL (xs:ls) (x:acc)
    sumL [] = 0
    sumL (x:xs) = x + sumL xs

myReverse :: [t] -> [t]
myReverse [] = []
myReverse l = rev l []
  where
    rev [] ys = ys
    rev (x:xs) ys = rev xs $ x:ys

-- Tower of Hanoi

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi discs from to tmp =
  (hanoi (discs - 1) from tmp to) ++ [(from, to)] ++ (hanoi (discs - 1) tmp to from)
    
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 discs from to tmp1 tmp2 = undefined
