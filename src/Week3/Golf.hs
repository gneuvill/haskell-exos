
module Week3.Golf where

skips :: [a] -> [[a]]
skips as = map (map snd . filterNth . fst) ias
  where
    ias = zip [1..] as
    filterNth n = filter (isNth n . fst) ias
    isNth n = (== 0) . (flip mod n)
