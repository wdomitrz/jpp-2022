{-# LANGUAGE FlexibleContexts #-}
module Constraints where
import Control.Monad.Error.Class
import Data.List(union, intersect, nub, (\\), deleteBy, intercalate)
import Text.Show(showListWith)

-- Using ideas from "Typing Haskell in Haskell" by Mark P. Jones

type Tyvar = String

infixr 5 :->
data Type = TInt | Type :-> Type | TVar Tyvar
  deriving Eq

instance Show Type where
  showsPrec d (TVar t) = showString t
  showsPrec d TInt = showString "int"
  showsPrec d (u :-> v) = showParen (d > arr_prec) $
             showsPrec (arr_prec+1) u .
             showString " -> "       .
             showsPrec arr_prec v
          where arr_prec = 5

data Scheme = Forall [Tyvar] Type
instance Show Scheme where
    showsPrec d (Forall as t) = showString "forall". shows as . showString "." . shows t

type Constraint = (Type,Type)
type Constraints = [Constraint]

newtype Subst = Subst [(Tyvar,Type)]
instance Show Subst where
  -- show (Subst ps) = intercalate "," (map showPair ps) where
  showsPrec _ (Subst ps) = showListWith showsPair ps where
    showsPair :: (Tyvar,Type) -> ShowS
    showsPair (v, t) = showString v .  showString ":=" . shows t

emptySubst :: Subst
emptySubst = Subst []

(+->)      :: Tyvar -> Type -> Subst
u +-> t     = Subst [(u, t)]

expel :: [Tyvar] -> Subst -> Subst
expel [] s = s
expel (v:vs) (Subst s) = Subst $ filter ((v/=).fst) s

class HasTypes a where
    apply :: Subst -> a -> a
    ftv   :: a -> [Tyvar]

instance HasTypes Type where
    apply (Subst s) t@(TVar u) = case lookup u s of
      Nothing -> t
      Just v -> v
    apply s (l:->r) = apply s l :-> apply s r
    apply s t = t

    -- ftv :: Type -> [Tyvar]
    ftv (TVar u) = [u]
    ftv (l:->r)  = ftv l `union` ftv r
    ftv _ = []

instance HasTypes Scheme where
  apply subst (Forall tvs t) = Forall tvs (apply (expel tvs subst) t)
  ftv (Forall tvs t) = ftv t \\ tvs

-- this isn't really used, just for experiments
instance HasTypes Subst where
  apply s1 (Subst ps2) = Subst [ (u, apply s1 t) | (u,t) <- ps2 ]
  ftv s = error "ftv not implemented for Subst"
  
-- infixr 5 @@
-- (@@) :: Subst -> Subst -> Subst

instance Semigroup Subst where
  -- apply (s1 <> s2) = apply s1 . apply s2
  s1@(Subst ps1) <> s2@(Subst ps2) =  Subst $ [ (u, apply s1 t) | (u,t) <- ps2 ] ++ ps1

instance Monoid Subst where
  mempty = emptySubst

mgu     :: MonadError String m => Type -> Type -> m Subst
varBind :: MonadError String m => Tyvar -> Type -> m Subst


{-
   Solving (l:->r) ~ (l':->r') amounts to solving [ l ~ l', r ~ r']:
   s1 <- l ~ l'
   apply s1 to rest of the equations (i.e. r ~ r')
   s2 <- (s1 r ~ s1 r')
   return (s2 <> s1)

   IOW

   solve [] = mempty
   solve ((l:->r, l':->r'):es) = solve ((l,l'):(r,r'):es)
   solve ((TInt,TInt):es) = solve es
   solve ((TVar u, t):es) = do { s1 <- varBind u t; s2 <-solve (apply s1 es); return s2<>s1 }
-}
mgu (l:->r) (l':->r') = do s1 <- mgu l l'
                           s2 <- mgu (apply s1 r) (apply s1 r')
                           return (s2 <> s1)
mgu (TVar u) t        = varBind u t
mgu t (TVar u)        = varBind u t
mgu (TInt) (TInt)     = return mempty
mgu t1 t2             = throwError (unwords
                                    ["types",
                                     quote t1,
                                     "and",
                                     quote t2,
                                     "do not unify"])


quote :: Show a => a -> String
quote t = "'" ++ show t ++ "'"

varBind u t | t == TVar u      = return emptySubst
            | u `elem` ftv t   = throwError (errInfinite (TVar u) t)
            | otherwise        = return (u +-> t)

errInfinite :: Type -> Type -> String
errInfinite u t = unwords [
  "Cannot construct the infinite type:",
  show u,
  "~",
  show t
  ]


type E a = Either String a
testmgu :: Type -> Type -> E Subst
testmgu = mgu
