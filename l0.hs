import Data.List (permutations)

head' :: [a] -> a
head' (h : _) = h
head' [] = undefined

tail' :: [a] -> [a]
tail' (_ : t) = t
tail' [] = undefined

(+++) :: [a] -> [a] -> [a]
(+++) [] b = b
(+++) (x : xs) ys = x : (xs +++ ys)

(++++) :: [a] -> [a] -> [a]
(++++) [] = id
(++++) (x : xs) = \ys -> x : (xs +++ ys)

-- (+++) (x : xs) ys = x : (+++) xs ys

take' :: [a] -> Integer -> [a]
take' _ 0 = []
take' [] _ = []
take' (x : xs) n = x : take' xs (n -1)

drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' 0 a = a
drop' n (x : xs) = drop (n -1) xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs) = if f x then x : res else res
  where
    res = filter' f xs

-- filter' f (x : xs)
--   | f x = x : res
--   | otherwise = res
--   where
--     res = filter' f xs

map' _ [] = []
map' f (x : xs) = f x : map' f xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' [x] = x
concat' (x : xs) = x ++ concat' xs

concat'' [] = []
concat'' [x] = x
concat'' ([] : xs) = concat'' xs
concat'' ((x : xs) : ys) = x : concat'' (xs : ys)

inits :: [a] -> [[a]]
inits [] = [[]]
inits (h : t) = [] : map (h :) (inits t)

partitions :: [a] -> [([a], [a])]
partitions [] = [([], [])]
partitions l@(h : t) = ([], l) : map (\(x, y) -> (h : x, y)) (partitions t)

permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' (h : t) = concat' (map (insert h) (permutations' t))
  where
    insert h [] = [[h]]
    insert h (x : xs) = (h : x : xs) : map (x :) (insert h xs)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = x : filter (/= x) (nub xs)

-- nub (x : xs) = x : nub (filter (/= x) xs)

(!=) :: a -> a -> Bool
(!=) = error "not implemented"
