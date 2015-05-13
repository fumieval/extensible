{-# LANGUAGE LambdaCase, TypeFamilies #-}
------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Nullable
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
------------------------------------------------------------------------
module Data.Extensible.Nullable (
  coinclusion
  , wrench
  , retrench
  , Nullable(..)
  , mapNullable) where

import Data.Extensible.Class
import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Extensible.Inclusion
import Data.Extensible.Internal.Rig
import Data.Typeable (Typeable)
import Data.Monoid
import Data.Extensible.Wrapper
import Data.Profunctor.Unsafe

-- | Poly-kinded Maybe
newtype Nullable h x = Nullable { getNullable :: Maybe (h x) } deriving (Show, Eq, Ord, Typeable)

instance Wrapper h => Wrapper (Nullable h) where
  type Repr (Nullable h) x = Maybe (Repr h x)
  _Wrapper = withIso _Wrapper $ \f g -> dimap (fmap f . getNullable) (fmap (Nullable . fmap g))

-- | Apply a function to its content.
mapNullable :: (g x -> h y) -> Nullable g x -> Nullable h y
mapNullable f = Nullable #. fmap f .# getNullable
{-# INLINE mapNullable #-}

-- | The inverse of 'inclusion'.
coinclusion :: (Include ys xs, Generate ys) => Nullable (Membership xs) :* ys
coinclusion = flip appEndo (htabulate $ const $ Nullable Nothing)
  $ hfoldMap getConst'
  $ hmapWithIndex (\src dst -> Const' $ Endo $ pieceAt dst `over` const (Nullable $ Just src))
  $ inclusion

-- | Extend a product and fill missing fields by 'Null'.
wrench :: (Generate ys, xs ⊆ ys) => h :* xs -> Nullable h :* ys
wrench xs = mapNullable (flip hlookup xs) `hmap` coinclusion
{-# INLINE wrench #-}

-- | Narrow the range of the sum, if possible.
retrench :: (Generate ys, xs ⊆ ys) => h :| ys -> Nullable ((:|) h) xs
retrench (EmbedAt i h) = views (pieceAt i) (mapNullable (`EmbedAt`h)) coinclusion
{-# INLINE retrench #-}
