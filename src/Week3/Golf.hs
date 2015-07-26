{-# OPTIONS_GHC -Wall #-}

module Week3.Golf where

skips :: [a] -> [[a]]
skips as = map (map snd . filterNth . fst) ias
  where
    ias = zip [1::Integer ..] as -- `as` zipped with index starting at 1
    filterNth n = filter (isNth n . fst) ias -- every 'nth' element of `ias`
    isNth n = (== 0) . (flip mod n) -- is a given number a multiple of `n` ?

localMaxima :: [Integer] -> [Integer]
localMaxima xs
  | length xs < 3 = [] -- make sure the input list counts at least three elts
  | otherwise = map getSnd $ filter isLocMax predsAndSuccs
  where
    center = init $ tail xs -- all but the head and the last elt of xs
    predsAndSuccs = zip3 xs center (tail $ tail xs) -- a list of triplet (pred, x, succ) based on xs
    isLocMax (a, b, c) = a <= b && b >= c -- does a given triplet identify a local maximum ?
    getSnd (_, b, _) = b -- get the second elt of a triplet

