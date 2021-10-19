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
import Data.Functor.Compose
import Data.Extensible.Class
import Data.Extensible.Field
import Data.Extensible.Product
import Data.Extensible.Internal.Rig
import Data.Extensible.Nullable
import Data.Extensible.Wrapper

-- | @'TangleT' h xs m@ is the monad of computations that may depend on the elements in 'xs'.
newtype TangleT xs h m a = TangleT
  { unTangleT :: RWST (xs :& Compose (TangleT xs h m) h) () (xs :& Nullable h) m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans (TangleT xs h) where
  lift = TangleT . lift

instance (Monad m, Semigroup a) => Semigroup (TangleT xs h m a) where
  (<>) = liftA2 (<>)

instance (Monad m, Monoid a) => Monoid (TangleT xs h m a) where
  mempty = pure mempty
  mappend = (<>)

-- | Hitch an element associated to the 'FieldName' through a wrapper.
lasso :: forall k v m h xs. (Monad m, Lookup xs k v, Wrapper h)
  => FieldName k -> TangleT xs h m (Repr h (k ':> v))
lasso _ = view _Wrapper <$> hitchAt (association :: Membership xs (k ':> v))
{-# INLINE lasso #-}

-- | Take a value from the tangles. The result is memoized.
hitchAt :: Monad m => Membership xs x -> TangleT xs h m (h x)
hitchAt k = TangleT $ do
  mem <- get
  case getNullable $ hlookup k mem of
    Just a -> return a
    Nothing -> do
      tangles <- ask
      a <- unTangleT $ getCompose $ hlookup k tangles
      modify $ over (pieceAt k) $ const $ Nullable $ Just a
      return a

-- | Run a 'TangleT' action and return the result and the calculated values.
runTangleT :: Monad m
  => xs :& Compose (TangleT xs h m) h
  -> xs :& Nullable h
  -> TangleT xs h m a
  -> m (a, xs :& Nullable h)
runTangleT tangles rec0 (TangleT m) = (\(a, s, _) -> (a, s))
  <$> runRWST m tangles rec0
{-# INLINE runTangleT #-}

-- | Run a 'TangleT' action.
evalTangleT :: Monad m
  => xs :& Compose (TangleT xs h m) h
  -> xs :& Nullable h
  -> TangleT xs h m a
  -> m a
evalTangleT tangles rec0 (TangleT m) = fst <$> evalRWST m tangles rec0
{-# INLINE evalTangleT #-}

-- | Run tangles and collect all the results as a 'Record'.
runTangles :: Monad m
  => xs :& Compose (TangleT xs h m) h
  -> xs :& Nullable h
  -> m (xs :& h)
runTangles ts vs = evalTangleT ts vs $ htraverseWithIndex (const . hitchAt) vs
{-# INLINE runTangles #-}
