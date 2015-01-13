{-# LANGUAGE Rank2Types, DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.League
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Efficient extensible functor
------------------------------------------------------------------------
module Data.Extensible.League where

import Data.Extensible.Internal
import Data.Extensible.Sum
import Data.Extensible.Product
import Data.Extensible.Match
import Data.Typeable

-- | A much more efficient representation for 'Union' of 'Functor's.
newtype League fs a = League { getLeague :: Fuse a :| fs } deriving Typeable

-- | fast fmap
instance Functor (League fs) where
  fmap f (League (UnionAt pos s)) = League (UnionAt pos (mapFuse f s))
  {-# INLINE fmap #-}

-- | /O(log n)/ Embed a functor.
liftL :: (Functor f, f ∈ fs) => f a -> League fs a
liftL f = League $ embed $ Fuse $ \g -> fmap g f
{-# INLINE liftL #-}

-- | Flipped <http://hackage.haskell.org/package/kan-extensions-4.1.0.1/docs/Data-Functor-Yoneda.html Yoneda>
newtype Fuse a f = Fuse { getFuse :: forall b. (a -> b) -> f b }

-- | Fuse 'Fuse' to retract a substantial functor.
meltdown :: Fuse a f -> f a
meltdown (Fuse f) = f id
{-# INLINE meltdown #-}

-- | 'fmap' for the content.
mapFuse :: (a -> b) -> Fuse a f -> Fuse b f
mapFuse f (Fuse g) = Fuse (\h -> g (h . f))
{-# INLINE mapFuse #-}

-- | Prepend a clause for @'Match' ('Fuse' x)@ as well as ('<?!').
(<?~) :: (f x -> a) -> Match (Fuse x) a :* fs -> Match (Fuse x) a :* (f ': fs)
(<?~) f = (<:*) (Match (f . meltdown))
{-# INLINE (<?~) #-}
infixr 1 <?~
