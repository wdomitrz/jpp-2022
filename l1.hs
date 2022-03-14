import Data.Maybe

incAll :: [[Int]] -> [[Int]]
incAll = map (map (+ 1))

inits :: [a] -> [[a]]
inits [] = [[]]
inits (h : t) = [] : map (h :) (inits t)

-- >>> incAll $ inits [1..3]
-- [[],[2],[2,3],[2,3,4]]

silnia :: Int -> Int
silnia n = foldr (*) 1 [1 .. n]

concat' :: [[a]] -> [a]
concat' = foldr (++) []

scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct x y = sum $ zipWith (*) x y

triplets :: Int -> [(Int, Int, Int)]
triplets n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n]]

triads :: Int -> [(Int, Int, Int)]
triads n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], z * x + y * y == z * z, x <= y, gcd x y == 1]

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe (Just x) = Just $ x + 1
incMaybe _ = Nothing

addMaybe :: Num a => Maybe a -> Maybe a -> Maybe a
addMaybe (Just x) (Just y) = Just $ x + y
addMaybe _ _ = Nothing

fib :: Int -> Int
fib n = fiba n 0 1
  where
    fiba 0 a1 _ = a1
    fiba n a1 a2 = fiba (n -1) a2 (a1 + a2)

pierwsze :: [Int]
pierwsze = go [2 ..]
  where
    go :: [Int] -> [Int]
    go (x : xs) = x : go (filter (\y -> y `mod` x /= 0) xs)
    go [] = []

-- >>> take 10 pierwsze
-- [2,3,5,7,11,13,17,19,23,29]

indexOf :: Char -> String -> Maybe Int
-- indexOf :: (Eq a1, Num a2, Enum a2) => a1 -> [a1] -> Maybe a2
indexOf c s = foldr (\(x, i) mi -> if x == c then Just i else mi) Nothing $ zip s [0 ..]

positions :: Char -> String -> [Int]
-- positions :: (Eq a1, Num a2, Enum a2) => a1 -> [a1] -> [a2]
positions c s = foldr (\(x, i) mi -> if x == c then i : mi else mi) [] $ zip s [0 ..]
