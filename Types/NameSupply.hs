module NameSupply (NS, namePool, split, deplete) where

addNumbers :: [String] -> [Integer] -> [String]
addNumbers names numbers = do
  number <- numbers
  name <- names
  return $ name ++ show number

namePool :: [String]
namePool = names ++ addNumbers names [1 ..]
  where
    names = ["a", "b", "c", "d"]

type NS = [String]

deplete :: NS -> (String, NS)
deplete (x : xs) = (x, xs)

split :: NS -> (NS, NS)
split (x : y : zs) = (x : xs, y : ys) where (xs, ys) = split zs

test1 = take 10 namePool

-- >>> test1
-- ["a","b","c","d","a1","b1","c1","d1","a2","b2"]
test2 = take 10 $ fst (split namePool)

-- >>> test2
-- ["a","c","a1","c1","a2","c2","a3","c3","a4","c4"]
test3 = fst (deplete ns) where ns = snd (deplete namePool)

-- >>> test3
-- "b"
