{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map, empty, insert, (!), (!?))
import Types.IntLambda

type Env = Map Name Type

typeOfInEnv :: Env -> Exp -> Either String Type
typeOfInEnv env (EInt _) = return TInt
typeOfInEnv env (EVar v) = return $ env ! v
typeOfInEnv env (ELam v t exp) = do
  -- \(v::t). exp
  texp <- typeOfInEnv (insert v t env) exp
  return $ t :-> texp
typeOfInEnv env (EApp f x) = do
  -- f x
  tf <- typeOfInEnv env f
  tx <- typeOfInEnv env x
  case tf of
    (:->) t1 t2 -> if t1 == tx then return t2 else Left ("type of: " ++ show f ++ " does not match type of: " ++ show x)
    _ -> Left ("type of: " ++ show f ++ " is not a function type")

typeOf :: Exp -> Either String Type
typeOf = typeOfInEnv empty

typeCheck exp = case typeOf exp of
  Right t -> t
  Left s -> error s

-- \x -> x x
--  x :: a
--  x :: a -> b
-- a ~ ((a) -> b)
-- a ~ ((a -> b) -> b)
-- a ~ (((a -> b) -> b) -> b)
-- (\x -> x x) (id :: a -> a)
--   (b->b) (a->a)
--   ((a->a)->(a->a)) (a->a)
-- (\x -> x x) (((.) :: (a -> b) -> (b -> c) -> a -> c)
-- (a -> b) -> (b -> c) -> a -> c
--          (a1 -> b1) -> (b1 -> c1) -> a1 -> c1
-- (a -> b) ~ (a1 -> b1) -> ((b1 -> c1) -> a1 -> c1)
-- a ~ (a1 -> b1)
-- b ~ (b1 -> c1) -> a1 -> c1
-- (\x -> x x) ((\x-> 7) :: (a -> Int))
-- (a -> Int)
--       (a1 -> Int)
-- a ~ a1 -> Int
-- ((a1 -> Int) -> Int)

-- >>> typeOf (EInt 42)
-- Right int

-- >>> typeCheck intK
-- int -> int -> int

--typeOfInEnv' :: Env -> Exp -> Either String Type
typeOfInEnv' :: (MonadReader Env m, MonadError String me) => Exp -> m (me Type)
typeOfInEnv' (EInt _) = return $ return TInt
--                      ^ m
--                               ^ Either
typeOfInEnv' (EVar x) = do
  env <- ask
  case env !? x of
    Just t -> return $ return t
    Nothing -> return $ throwError $ "Undeclared variable `" ++ x ++ "'"
typeOfInEnv' (ELam v t exp) = do
  -- \(v::t). exp
  --   env <- ask
  --   let etexp = runReader  (typeOfInEnv' exp) (insert v t env)
  etexp <- local (insert v t) (typeOfInEnv' exp)
  return $ do
    texp <- etexp
    return $ t :-> texp
typeOfInEnv' (EApp f x) = do
  -- f x
  -- (\y :: Int -> y + 1) (y + 5)
  etf <- typeOfInEnv' f
  etx <- typeOfInEnv' x
  return $ do
    tf <- etf
    tx <- etx
    case tf of
      (:->) t1 t2 -> if t1 == tx then return t2 else throwError ("type of: " ++ show f ++ " does not match type of: " ++ show x)
      _ -> throwError ("type of: " ++ show f ++ " is not a function type")

-- typeOfInEnv' (EApp f x) = do
--   -- f x
--   -- (\y :: Int -> y + 1) (y + 5)
--   env <- get
--   etf <- typeOfInEnv f
--   put env
--   etx <- typeOfInEnv x
--   put env
--   case tf of
--     (:->) t1 t2 -> if t1 == tx then return t2 else Left ("type of: " ++ show f ++ " does not match type of: " ++ show x)
--     _ -> Left ("type of: " ++ show f ++ " is not a function type")

typeOfInEnv'' :: MonadReader Env m => Exp -> ErrorT String m Type
typeOfInEnv'' (EInt _) = return TInt
typeOfInEnv'' (EVar x) = do
  env <- ask
  case env !? x of
    Just t -> return t
    Nothing -> throwError $ "Undeclared variable `" ++ x ++ "'"
typeOfInEnv'' (ELam v t exp) = do
  -- \(v::t). exp
  texp <- local (insert v t) (typeOfInEnv'' exp)
  return $ t :-> texp
typeOfInEnv'' (EApp f x) = do
  -- f x
  tf <- typeOfInEnv'' f
  tx <- typeOfInEnv'' x
  case tf of
    (:->) t1 t2 -> if t1 == tx then return t2 else throwError ("type of: " ++ show f ++ " does not match type of: " ++ show x)
    _ -> throwError ("type of: " ++ show f ++ " is not a function type")

-- A (B a) e ~ A (C d e) (F g)
-- (B a) ~ (C d e), e ~ (F g)
-- e ~ F g
-- B a ~ C d (F g)

-- A (C a a) e ~ A (C e d) (F g)
-- C a a ~ C (F g) d
-- a ~ F g, a ~ d
-- a ~ d
-- d ~ F g
-- A (C (F g) (F g)) (F g) ~ A (C (F g) (F g)) (F g)
-- A (C (F g) (F g)) (F g)

-- (f :: D e f) (x :: c)
-- D e f ~ ((->) c d)
