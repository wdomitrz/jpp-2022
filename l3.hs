import Control.Monad (when)
import Control.Monad.Error.Class
import Data.Char (chr, digitToInt, intToDigit, isHexDigit, ord)
import Data.Foldable (concat, foldr)
import Data.List (foldl)
import Data.Map (Map, (!?))
import Data.Time.Format.ISO8601 (yearFormat)
import GHC.IO (unsafePerformIO)
import System.Environment (getArgs)
import System.IO (getLine)
import Text.Read (readEither)

-- Zadanie 2.
-- a.
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

-- b.
evalList' :: [Exp] -> [Maybe Int]
-- evalList' :: Functor f => f Exp -> f (Maybe Int)
-- evalList' :: Maybe Exp -> Maybe (Maybe Into)
evalList' = fmap eval

evalList :: [Exp] -> Maybe [Int]
evalList = evalGen eval

-- c.
evalGen :: (Foldable f, Monad m) => (Exp -> m Int) -> f Exp -> m [Int]
evalGen e = Data.Foldable.foldr (f e) (return [])
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

-- d.
data Exp' = Val' Int | Div' Exp' Exp' | Var' String

type Env = Data.Map.Map String Int

eval'' :: Env -> Exp' -> Either String Int
eval'' _ (Val' x) = return x -- Just x
eval'' env (Div' x y) = do
  vx <- eval'' env x
  vy <- eval'' env y
  safediv' vx vy
eval'' env (Var' xn) = case env !? xn of
  Nothing -> Left $ "Unknown variable " ++ xn
  Just vx -> return vx

-- >>> eval'' (Data.Map.fromList [("x", 7)]) (Div' (Val' 42) (Var' "x"))
-- Right 6
-- >>> eval'' (Data.Map.fromList [("y", 7)]) (Div' (Val' 42) (Var' "x"))
-- Left "Unknown variable x"

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

-- 4.
-- a.
mainA :: IO ()
mainA = do
  args <- System.Environment.getArgs
  -- let len = length params
  mapM_ putStrLn args

-- return :: Monad m => a -> m a

-- b.
whileM :: Monad m => m Bool -> m ()
whileM obliczenie = do
  x <- obliczenie
  when x $ whileM obliczenie

mainB :: IO ()
mainB = whileM $ do
  putStrLn "What's your favorite programming language?"
  answer <- getLine
  return $ answer /= "Haskell"

-- c.

mainC :: IO ()
mainC = do
  args <- System.Environment.getArgs
  input <- case args of
    [] -> getContents
    (filePath : _) -> readFile filePath
  let (lines, words, chars) = (countLines input, countWords input, countChars input)
  putStrLn $ concat ["Lines: ", show lines, ", words: ", show words, ", characters: ", show chars]

countLines :: String -> Int
countLines = length . lines

countWords :: String -> Int
countWords = length . words

countChars :: String -> Int
countChars = length

-- Zadanie 3.

data ParseError = Err {location :: Int, reason :: String} deriving (Show)

type ParseMonad = Either ParseError

parseHexDigit :: Char -> Int -> ParseMonad Integer
parseHexDigit digit location
  | isHexDigit digit = return $ toInteger $ digitToInt digit
  | otherwise = Left $ Err {location = location, reason = digit : " is not a hex digit"}

parseHex :: String -> ParseMonad Integer
parseHex s = Data.List.foldl go (return 0) (zip s [1 ..])
  where
    go :: ParseMonad Integer -> (Char, Int) -> ParseMonad Integer
    go x (digit, pos) = do
      vx <- x
      digitValue <- parseHexDigit digit pos
      return $ 16 * vx + digitValue

toString :: Integer -> ParseMonad String
toString 0 = return "0"
toString x = return $ go x ""
  where
    go :: Integer -> String -> String
    go 0 acc = acc
    go x acc = go (x `div` 10) (intToDigit (fromInteger $ x `mod` 10) : acc)

-- convert zamienia napis z liczba szesnastkowa
--   na napis z liczba dziesietna
convert :: String -> String
convert s = str
  where
    (Right str) = tryParse s `catchError` printError
    tryParse s = do n <- parseHex s; toString n
    printError e = return $ concat ["Error at position ", show (location e), ": ", reason e]

-- >>> convert "3A7D"
-- "14973"
-- >>> convert "1aD0"
-- "6864"
-- >>> convert "12a3z1f23"
-- "Error at position 5: z is not a hex digit"

-- >>> convert "12a3z1n23"
-- "Error at position 5: z is not a hex digit"

-- Zadanie 1.

swapEither :: Either b a -> Either a b
swapEither (Left x) = Right x
swapEither (Right x) = Left x

-- | readInts2
-- >>> readInts2 "1 23 456 abc 9"
-- Left "Not a number: abc"
-- >>> readInts2' "1 23 456 abc 9"
-- Left "Not a number: abc"
readInts2 :: String -> Either String [Int]
readInts2 = mapM go . words
  where
    go w = swapEither $ swapEither (readEither w) >> return ("Not a number: " ++ w)

-- >>> readInts2 "1 23 456 abc 9"
-- Left "Not a number: abc"

sumInts :: String -> String
sumInts s = either id id $ do
  ints <- readInts2 s
  return $ show $ sum ints

-- >>> sumInts "1 23 456 abc 9"
-- >>> sumInts "1 2 3"
-- "Not a number: abc"
-- "6"
