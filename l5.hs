{-# LANGUAGE LambdaCase #-}

import Control.Monad.Trans (MonadTrans, lift)

newtype IdentityT m a = IdentityT {runIdentity :: m a}

-- Prelude> newtype IdentityT m a = IdentityT { runIdentity :: m a}
-- Prelude> :t IdentityT
-- IdentityT :: m a -> IdentityT m a
-- Prelude> :t runIdentity
-- runIdentity :: IdentityT m a -> m a

-- https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#t:Functor
instance Functor m => Functor (IdentityT m) where
  -- cel: fmap :: Functor m => (a -> b) -> IdentityT m a -> IdentityT m b
  -- mamy: fmap :: Functor m => (a -> b) -> m a -> m b
  fmap f i = IdentityT (f <$> runIdentity i {-:: m a-})

--                             wyjmij z pudełka
--                     przetwórz
--         zapakuj
-- https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:-62--62--61-
-- fmap :: Functor m => (a -> b) -> m a -> m b
-- (<*>) :: Applicative m => m (a -> b) -> m a -> m b
-- (=<<) :: Monad m => (a -> m b) -> m a -> m b

-- >>> (Just (+2)) <*> (Just 5)
-- Just 7

-- return :: m a

-- fmap f x = (pure f) <*> x
-- (<*>) f a = do
-- 	f' <- f -- f :: (m a), f' :: a
-- 	a' <- a
-- 	return $ f' a'
-- x :: m a
-- x >>= (\x' -> …)
-- (<*>) f a = f >>= (\f' -> (a >>= (\a' -> return $ f' a')))

instance Applicative m => Applicative (IdentityT m) where
  --   (<*>) :: Applicative m => IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
  f <*> i = IdentityT (runIdentity f <*> runIdentity i)
  pure x = IdentityT $ pure x

instance Monad m => Monad (IdentityT m) where
  -- cel: (>>=) :: Monad m => IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  -- mamy: (>>=) :: m a -> (a -> m b) -> m b
  -- (>>=) x f = let f' v = runIdentity (f v) -- v :: a, f v :: IdentityT m b, runIdentity (f v) :: m b
  --                 y = x >>= f' -- f :: a -> IdentityT m b
  --                    -- f' :: a -> m b
  --             in IdentityT y
  i >>= f = IdentityT (runIdentity i >>= runIdentity . f)

--                   wyjmij z pudełka
--                     		         przygotuj funkcję do przetwarzania
--                                 przetwórz
--        zapakuj

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

-- Jeśli m jest funktorem, to (m . Maybe) też
--    m a                      m (Maybe a)
instance Functor m => Functor (MaybeT m) where
  -- fmap :: (a -> b) -> m a -> m b -- dla tego m z góry
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  -- cel: fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  --   fmap f x = MaybeT (fmap f' x)
  --     where
  --       x' {-:: m (Maybe a)-} = runMaybeT x
  --       f' = fmap f
  -- f' :: Maybe a -> Maybe b
  fmap f x = MaybeT (fmap (fmap f) (runMaybeT x))

--                       ^ fmap na Maybe
--                 ^ fmap na m

instance MonadTrans IdentityT where
  lift = IdentityT

lift = IdentityT

-- lift :: m a -> IdentityT m a
-- lift :: MonadTrans t m a => m a -> t m a

-- https://hackage.haskell.org/package/conduit-1.3.4.2/docs/Conduit.html#t:MonadTrans
instance MonadTrans MaybeT where
  lift x = MaybeT (fmap Just x)

-- x :: m a, fmap Just x :: m (Maybe a), Just :: a -> Maybe a

-- instance Functor m => Functor (MaybeT m) where
--   fmap f x = MaybeT (fmap (fmap f) (runMaybeT x))

-- newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

--  MonadTrans (ExceptT e)
--  MonadTrans MaybeT
-- Needed for Monad
instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  (<*>) f x = MaybeT $ (<*>) <$> runMaybeT f <*> runMaybeT x

--                       ^ Maybe is an Applicative instance

instance Monad m => Monad (MaybeT m) where
  -- cel: (>>=) :: Monad m => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  -- mamy: (>>=) :: Monad m => m a -> (a -> m b) -> m b
  -- mamy: (>>=) :: Monad m => m (Maybe c) -> ((Maybe c) -> m b) -> m b

  -- https://hackage.haskell.org/package/conduit-1.3.4.2/docs/Conduit.html#t:MonadTrans
  --  MonadTrans (ErrorT e)
  -- return x = MaybeT $ return $ Just x
  -- (>>=) x {-:: MaybeT m a-} f {-:: a -> MaybeT m b-} =
  --   MaybeT $
  --     runMaybeT x {-:: m (Maybe a))-}
  --       >>= ( ( \v {-:: Maybe a-} -> case v of
  --                 Nothing -> return Nothing
  --                 Just vx -> runMaybeT (f vx)
  --             ) ::
  --               (Maybe a -> m (Maybe b))
  --           )
  (>>=) x f =
    MaybeT $
      runMaybeT x
        >>= ( \case
                Nothing -> return Nothing
                Just vx -> runMaybeT (f vx)
            )

-- https://moodle.mimuw.edu.pl/mod/page/view.php?id=81237
