data Tree a
  = Empty
  | Node a (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show Empty = ""
  show (Node x lt rt) = show x ++ " (" ++ show lt ++ "), (" ++ show rt ++ ")"

instance Eq a => Eq (Tree a) where
  Empty == Empty = True
  (Node x lt rt) == (Node x' lt' rt') = x == x' && lt == lt' && rt == rt'
  _ == _ = False

-- instance Eq (Tree a) where
--   Empty == Empty = True
--   (Node _ lt rt) == (Node _ lt' rt') = lt == lt' && rt == rt'
--   _ == _ = False

toList :: Tree a -> [a]
-- toList (Node v l r) = toList l ++ [v] ++ toList r
-- toList Empty = []

toList = go [] -- toList t = go [] t
  where
    go acc Empty = acc
    go acc (Node v l r) = go (v : go acc r) l

foldTree :: (t1 -> t2 -> t2) -> t2 -> Tree t1 -> t2
foldTree _ acc Empty = acc
foldTree f acc (Node v l r) = foldTree f (f v $foldTree f acc r) l

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node v l r)
  | x < v = Node v (insert x l) r
  | otherwise = Node v l (insert x r)

member :: (Ord a) => a -> Tree a -> Bool
member x Empty = False
member x (Node v l r)
  | x == v = True
  | x < v = member x l
  | otherwise = member x r

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Empty

sort :: Ord a => [a] -> [a]
sort = toList . fromList

data MyMaybe a = MyNothing | MyJust a

-- >>> MyJust 5
-- >>> MyJust (MyJust 5)
-- >> MyJust MyNothing
