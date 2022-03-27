import Control.Monad
import Data.Time.Format.ISO8601 (yearFormat)
import GHC.IO (unsafePerformIO)
import System.Environment (getArgs)

data Exp = Val Int | Div Exp Exp

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (div x y)

eval :: Exp -> Maybe Int
eval (Val x) = return x -- Just x
eval (Div x y) = do
  vx <- eval x
  vy <- eval y
  safediv vx vy

evalList' :: [Exp] -> [Maybe Int]
-- evalList' :: Functor f => f Exp -> f (Maybe Int)
-- evalList' :: Maybe Exp -> Maybe (Maybe Into)
evalList' = fmap eval

evalList :: [Exp] -> Maybe [Int]
evalList = evalGen eval

evalGen :: (Foldable f, Monad m) => (Exp -> m Int) -> f Exp -> m [Int]
evalGen e = foldr (f e) (return [])
  where
    f :: Monad m => (Exp -> m Int) -> Exp -> m [Int] -> m [Int]
    f howEval x l = do
      -- pierwszy błąd od początku
      vx <- howEval x
      vl <- l
      return $ vx : vl

    f' howEval x l = do
      -- pierwszy błąd od końca
      vl <- l
      vx <- howEval x
      return $ vx : vl

f x l = eval x >>= (\vx -> l >>= (\vl -> return $ vx : vl))

safediv' :: Int -> Int -> Either String Int
safediv' _ 0 = Left "Division by zero"
safediv' x y = Right (div x y)

eval' :: Exp -> Either String Int
eval' (Val x) = return x -- Just x
eval' (Div x y) = do
  vx <- eval' x
  vy <- eval' y
  safediv' vx vy

evalListEither :: [Exp] -> Either String [Int]
evalListEither = evalGen eval'

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- (=<<) :: Monad f => (a -> f b) -> f a -> f b
-- (>>=) :: Monad f => f a -> (a -> f b) -> f b

-- System.Environment.getArgs :: IO [String]
-- main = putStr "Hello " >> putStrLn "Wolrd!"
-- main =
--   ( do
--       putStr "Hello "
--       putStrLn "Wolrd!"
--       return 1
--   ) ::
--     IO Int
-- main = System.Environment.getArgs >>= mapM_ putStrLn

main = do
  params <- System.Environment.getArgs
  let len = length params
  mapM_ putStrLn params

-- return :: Monad m => a -> m a

whileM :: Monad m => m Bool -> m ()
whileM obliczenie = do
  x <- obliczenie
  when x $ whileM obliczenie

data ParseError = Err {location :: Int, reason :: String}

type ParseMonad a = Either ParseError a
