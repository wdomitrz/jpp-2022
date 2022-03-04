countdown 0 = [0]
countdown n = n : countdown (n -1)

-- >>> countdown 9
-- [9,8,7,6,5,4,3,2,1,0]

collatz 1 = [1]
collatz n
  | even n = n : collatz (n `div` 2)
  | otherwise = n : collatz (3 * n + 1)

-- >>> collatz 17
-- [17,52,26,13,40,20,10,5,16,8,4,2,1]

last' :: [a] -> a
last' [x] = x
last' (_ : xs) = last xs
last' [] = undefined

-- >>> last' [1, 2, 3]
-- 3

head' :: [a] -> a
head' (h : _) = h
head' [] = undefined

-- >>> head' [1, 2, 3]
-- 1

tail' :: [a] -> [a]
tail' (_ : t) = t
tail' [] = undefined

-- >>> tail' [1, 2, 3]
-- [2,3]

(+++) :: [a] -> [a] -> [a]
(+++) [] b = b
(+++) (x : xs) ys = x : (xs +++ ys)

-- >>> [1, 2, 3] +++ [4, 5]
-- [1,2,3,4,5]

(++++) :: [a] -> [a] -> [a]
(++++) [] = id
(++++) (x : xs) = \ys -> x : (xs +++ ys)

-- >>> [1, 2, 3] ++++ [4, 5]
-- [1,2,3,4,5]

-- (+++) (x : xs) ys = x : (+++) xs ys

take' :: [a] -> Integer -> [a]
take' _ 0 = []
take' [] _ = []
take' (x : xs) n = x : take' xs (n -1)

-- >>> take' [1, 2, 3] 2
-- [1,2]
-- >>> take' [1, 2, 3] 4
-- [1,2,3]

drop' :: Integer -> [a] -> [a]
drop' n [] = []
drop' 0 a = a
drop' n (x : xs) = drop' (n -1) xs

-- >>> drop' 2 [1, 2, 3, 4, 5]
-- [3,4,5]
-- >>> drop' 4 [1, 2, 3]
-- []

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs) = if f x then x : res else res
  where
    res = filter' f xs

-- >>> filter' odd [1, 2, 3]
-- [1,3]

-- filter' f (x : xs)
--   | f x = x : res
--   | otherwise = res
--   where
--     res = filter' f xs

map' :: (t -> a) -> [t] -> [a]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

-- >>> map' (+1) [1, 2, 3]
-- [2,3,4]

concat' :: [[a]] -> [a]
concat' [] = []
concat' [x] = x
concat' (x : xs) = x ++ concat' xs

-- >>> concat' [[1, 2, 3], [4, 5], [6], []]
-- [1,2,3,4,5,6]

concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' [x] = x
concat'' ([] : xs) = concat'' xs
concat'' ((x : xs) : ys) = x : concat'' (xs : ys)

-- >>> concat'' [[1, 2, 3], [4, 5], [6], []]
-- [1,2,3,4,5,6]

inits :: [a] -> [[a]]
inits [] = [[]]
inits (h : t) = [] : map (h :) (inits t)

-- >>> inits [1, 2]
-- [[],[1],[1,2]]

partitions :: [a] -> [([a], [a])]
partitions [] = [([], [])]
partitions l@(h : t) = ([], l) : map (\(x, y) -> (h : x, y)) (partitions t)

-- >>> partitions [1, 2, 3]
-- [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])]

permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' (h : t) = concat' (map (insert h) (permutations' t))
  where
    insert h [] = [[h]]
    insert h (x : xs) = (h : x : xs) : map (x :) (insert h xs)

-- >>> permutations' [1, 2, 3]
-- [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

nub :: Eq a => [a] -> [a]
nub [] = []
-- nub (x : xs) = x : nub (filter (/= x) xs)
nub (x : xs) = x : filter (/= x) (nub xs)

-- >>> nub [1,2,1,3,1,2,1,4]
-- [1,2,3,4]
