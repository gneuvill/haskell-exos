module Week6.Fibonacci where

-- Exo 1

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exo 2

fibs2 :: [Integer]
fibs2 = map fib2 ([0..] :: [Integer])
  where
    fib2 n = fst $ foldl (\(a,b) _ -> (b,a+b)) (0,1) [1..n]

fibs3 :: [Integer]
fibs3 = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)

fibs4 :: [Integer]
fibs4 = fib3 1 0
  where
    fib3 a b = b : fib3 (a + b) a

niceFibs :: [Integer]
niceFibs = 0 : 1 : zipWith (+) niceFibs (tail niceFibs)

-- Exo 3

data Stream a = a :+ (Stream a)
infixr 5 :+

streamToList :: Stream a -> [a]
streamToList (a :+ as) = a : streamToList as

instance Show a => Show (Stream a) where
  show =  (++ ",...]") . init . show . prefix 
    where prefix = take 20 . streamToList

-- Exo 4

streamRepeat :: a -> Stream a
streamRepeat a = a :+ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (a:+as) = f a :+ streamMap f as

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = a :+ streamFromSeed f (f a)

-- Exo 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleave :: Stream a -> Stream a -> Stream a
interleave (x :+ xs) ys = x :+ interleave ys xs

ruler :: Stream Integer
ruler = rulerLoop 0
  where
    rulerLoop n = let ns = streamRepeat n
                  in interleave ns (rulerLoop $ n + 1)

rulerMaths :: Stream Integer
rulerMaths = streamMap f natsFrom1
  where
    f n = head $ reverse $ takeWhile ((== 0) . (mod n) . (2^)) [0..]
    natsFrom1 = streamFromSeed (+1) 1  :: Stream Integer

testRuler :: Int -> Bool
testRuler n = toSizedList ruler == toSizedList rulerMaths
  where
    toSizedList = (take n) . streamToList
