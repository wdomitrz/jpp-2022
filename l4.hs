{-# LANGUAGE FlexibleContexts #-}

-- Jako ciekawostka, bo było pytanie na zajęciach:
-- http://pointfree.io/

import Control.Monad.Reader (MonadReader (ask), asks, runReader)
import Data.Map

allPairs :: [a] -> [a] -> [[a]]
allPairs xs ys = do
  x <- xs
  y <- ys
  return [x, y] -- [[x, y]]

allCombinations :: [[a]] -> [[a]]
allCombinations [] = [[]]
-- allCombinations (xs : xss) = [x : xs' | x <- xs, xs' <- xss]
allCombinations (xs : xss) = do
  xs' <- allCombinations xss -- (xss :: [[a]] = m [a])
  x <- xs -- (xs :: [a] = m a)
  return $ x : xs'

-- >>> allCombinations [[1,2], [4,5], [6], [7]]
-- [[1,4,6,7],[1,5,6,7],[2,4,6,7],[2,5,6,7]]

-- instance Foldable (f a) => Monad (f a) where
--   return x = [x]
--   xs >>= f = concatMap f xs

-- MonadReader r a = (r -> a)
-- runReader :: (r -> a) -> r -> a
-- runReader f v = f v
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

renumber :: Tree a -> Tree Int
-- renumber r = (renumberFrom t) 1
renumber t = runReader (renumberFrom t) 1

renumberFrom :: MonadReader Int m => Tree a -> m (Tree Int)
renumberFrom Empty = return Empty
renumberFrom (Node _ l r) = do
  x <- ask
  let ln = runReader (renumberFrom l) (x + 1)
  let rn = runReader (renumberFrom r) (x + 1)
  return $ Node x ln rn

type Var = String

data Exp
  = EInt Int
  | EOp Op Exp Exp
  | EVar Var
  | ELet Var Exp Exp -- let var = e1 in e2

data Op = OpAdd | OpMul | OpSub

type Env = Map Var Int

evalExpWith :: MonadReader Env m => Exp -> m Int
evalExpWith (EOp o e1 e2) = do
  v1 <- evalExpWith e1
  v2 <- evalExpWith e2
  let op = case o of
        OpAdd -> (+)
        OpMul -> (*)
        OpSub -> (-)
  return $ v1 `op` v2
evalExpWith (EInt x) = do
  return x
evalExpWith (EVar x) = do
  asks (! x)
evalExpWith (ELet var e1 e2) = do
  -- let var = e1 in e2
  v1 <- evalExpWith e1
  asks (runReader (evalExpWith e2) . insert var v1)
