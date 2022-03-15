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
-- >>> MyJust MyNothing
-- Maybe 5
-- Maybe (Maybe 5)
-- Maybe Nothing

-- 2 + 3 * 4
-- 2 + (3 * 4)
-- 2^5 * 4^10 + 3^6 * 7 + 1
-- ((((2^5) * (4^10)) + ((3^6) * 7)) + 1)
-- (2^5) * 3 == 2^5 * 3
-- (2 + 5) * 3 != 2 + 5 * 3
-- (2 + 3 * 5) ^ 4 != (2 + (3 * 5)) ^ 4

-- type ShowS = String -> String
-- showParen :: Bool -> ShowS -> ShowS
-- showParen False s = s
-- showParen True s = shows "(" . s . shows ")"

instance Show a => Show (MyMaybe a) where
  showsPrec _ MyNothing = ("Nothing" ++)
  showsPrec d (MyJust v) = showParen (d > myD) $ (shows "Maybe ") . showsPrec (myD + 1) v
    where
      myD = 5

newtype OrderedList a = OL [a]

instance Ord a => Semigroup (OrderedList a) where
  (<>) (OL x) (OL y) = OL $ merge x y
    where
      merge [] x' = x'
      merge x' [] = x'
      merge zzs@(z : zs) vvs@(v : vs)
        | z < v = z : merge zs vvs
        | otherwise = v : merge zzs vs

instance Ord a => Monoid (OrderedList a) where
  mempty = OL []

nubOrdered :: Ord a => OrderedList a -> OrderedList a
nubOrdered (OL x) = OL $ foldr go [] x
  where
    go x [] = [x]
    go x yys@(y : _)
      | x == y = yys
      | otherwise = x : yys

data Tree' a
  = Empty'
  | Leaf' a
  | Node' a (Tree a) (Tree a)

-- elimTree' :: (c) -> (a -> c) -> (a -> Tree a -> Tree a -> c) -> Tree' a -> c
-- foldTree' :: (c) -> (a -> c) -> (a -> c -> c -> c) -> Tree' a -> c

--             (:)                []
-- elimList :: (a -> [a] -> c) -> (c) -> [a] -> c
--             (:)              []
-- foldList :: (a -> c -> c) -> (c) -> [a] -> c

elimMaybe :: c -> (a -> c) -> Maybe a -> c
elimMaybe x _ Nothing = x
elimMaybe _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
-- fromMaybe = (`elimMaybe` id)
-- flip f x y = f y x
fromMaybe = flip elimMaybe id

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
--                             (\x -> Just $ f x)
mapMaybe f = elimMaybe Nothing (Just . f)

maybeHead :: [a] -> Maybe a
maybeHead = foldr (\h _ -> Just h) Nothing

elimEither :: (a -> c) -> (b -> c) -> Either a b -> c
elimEither f _ (Left x) = f x
elimEither _ f (Right x) = f x

mapEither :: (a1 -> a2) -> (b1 -> b2) -> Either a1 b1 -> Either a2 b2
mapEither fl fr = elimEither (Left . fl) (Right . fr)

mapRight :: (a1 -> a2) -> Either e a1 -> Either e a2
mapRight = mapEither id

fromEither :: Either a a -> a
fromEither = elimEither id id

reverseRight :: Either e [a] -> Either e [a]
reverseRight = mapRight reverse
