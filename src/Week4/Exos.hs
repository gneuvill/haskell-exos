{-# OPTIONS_GHC -Wall #-}

module Week4.Exos where

-- ## Exo 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate f
  where
    f n = if even n then n `div` 2 else 3 * n + 1

-- ## Exo 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where
    insert :: a -> Tree a -> Tree a
    insert a Leaf = Node 0 Leaf a Leaf
    insert a (Node _ t1 a0 t2) =
      if insertRight
      then Node newHeight t1 a0 newSubTree
      else Node newHeight newSubTree a0 t2
      where
        insertRight = getHeight t1 > getHeight t2
        newSubTree = insert a $ if insertRight then t2 else t1
        newHeight =
          1 + max (getHeight newSubTree) (getHeight $ if insertRight then t1 else t2)
    getHeight :: Tree a -> Integer
    getHeight Leaf = -1
    getHeight (Node h _ _ _) = h

-- ## Exo 3

xor :: [Bool] -> Bool
xor = odd . length . trues
  where
    trues = foldr (\b acc -> if b then b:acc else acc) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f a bs = foldr (flip f) a $ reverse bs

-- ## Exo 4 (Sieve of Sundaram)

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map  ((+1) . (*2)) . toKeep
  where
    toKeep n = [i | i <- [1..n], not $ i `elem` toDiscard n]
    toDiscard n = filter (<= n) [i + j + 2*i*j | i <- [1..n], j <- [i..n]]
