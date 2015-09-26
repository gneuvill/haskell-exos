module Week4.Exos where

-- ## Exo 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs) | even x    = (x - 2) * fun1 xs
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
    f n | even n = n `div` 2
        | otherwise = 3 * n + 1

-- ## Exo 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where
    insert a Leaf = Node 0 Leaf a Leaf
    insert a (Node _ t1 a0 t2)
      | getHeight t1 > getHeight t2 = newTree t1 a0 (insert a t2)
      | otherwise = newTree (insert a t1) a0 t2
    newTree t b t' =
      let newHeight = 1 + max (getHeight t) (getHeight t')
      in Node newHeight t b t'
    getHeight Leaf = -1
    getHeight (Node h _ _ _) = h

-- ## Exo 3

xor :: [Bool] -> Bool
xor = odd . length . trues
  where
    trues = foldr noFalse []
    noFalse b | b = (b :)
              | otherwise = id

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
