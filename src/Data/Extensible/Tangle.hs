{-# LANGUAGE ScopedTypeVariables #-}
------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Struct
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Extensible tangles
------------------------------------------------------------------------
module Data.Extensible.Tangle
  ( TangleT(..)
  , lasso
  , hitchAt
  , runTangleT
  , evalTangleT
  , runTangles
  ) where

import Control.Applicative
import Control.Monad.Trans.RWS.Strict
import Control.Monad.Trans.Class
import Data.Extensible.Class
import Data.Extensible.Field
import Data.Extensible.Product
import Data.Extensible.Internal.Rig
import Data.Extensible.Nullable
import Data.Extensible.Wrapper

-- | @'TangleT' h xs m@ is the monad of computations that may depend on the elements in 'xs'.
newtype TangleT h xs m a = TangleT
  { unTangleT :: RWST (Comp (TangleT h xs m) h :* xs) () (Nullable h :* xs) m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans (TangleT h xs) where
  lift = TangleT . lift

instance (Monad m, Semigroup a) => Semigroup (TangleT h xs m a) where
  (<>) = liftA2 (<>)

instance (Monad m, Monoid a) => Monoid (TangleT h xs m a) where
  mempty = pure mempty
  mappend = liftA2 mappend

-- | Hitch an element associated to the 'FieldName' through a wrapper.
lasso :: forall k v m h xs. (Monad m, Associate k v xs, Wrapper h)
  => FieldName k -> TangleT h xs m (Repr h (k ':> v))
lasso _ = view _Wrapper <$> hitchAt (association :: Membership xs (k ':> v))
{-# INLINE lasso #-}

-- | Take a value from the tangles. The result is memoized.
hitchAt :: Monad m => Membership xs x -> TangleT h xs m (h x)
hitchAt k = TangleT $ do
  mem <- get
  case getNullable $ hlookup k mem of
    Just a -> return a
    Nothing -> do
      tangles <- ask
      a <- unTangleT $ getComp $ hlookup k tangles
      modify $ over (pieceAt k) $ const $ Nullable $ Just a
      return a

-- | Run a 'TangleT' action and return the result and the calculated values.
runTangleT :: Monad m
  => Comp (TangleT h xs m) h :* xs
  -> Nullable h :* xs
  -> TangleT h xs m a
  -> m (a, Nullable h :* xs)
runTangleT tangles rec0 (TangleT m) = (\(a, s, _) -> (a, s))
  <$> runRWST m tangles rec0
{-# INLINE runTangleT #-}

-- | Run a 'TangleT' action.
evalTangleT :: Monad m
  => Comp (TangleT h xs m) h :* xs
  -> Nullable h :* xs
  -> TangleT h xs m a
  -> m a
evalTangleT tangles rec0 (TangleT m) = fst <$> evalRWST m tangles rec0
{-# INLINE evalTangleT #-}

-- | Run tangles and collect all the results as a 'Record'.
runTangles :: Monad m
  => Comp (TangleT h xs m) h :* xs
  -> Nullable h :* xs
  -> m (h :* xs)
runTangles ts vs = evalTangleT ts vs $ htraverseWithIndex (const . hitchAt) vs
{-# INLINE runTangles #-}
