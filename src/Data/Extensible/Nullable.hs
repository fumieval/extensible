{-# LANGUAGE Safe #-}
module Data.Extensible.Nullable (
  coinclusion
  , wrench
  , retrench
  , Nullable(..)
  , nullable
  , mapNullable) where

import Data.Extensible.Class
import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Extensible.Inclusion
import Data.Extensible.Internal.Rig
import Data.Typeable (Typeable)
import Data.Monoid
import Data.Extensible.Wrapper

-- | Poly-kinded Maybe
data Nullable h x = Null | Eine (h x) deriving (Show, Eq, Ord, Typeable)

-- | Destruct 'Nullable'.
nullable :: r -> (h x -> r) -> Nullable h x -> r
nullable r _ Null = r
nullable _ f (Eine h) = f h
{-# INLINE nullable #-}

-- | Apply a function to its content.
mapNullable :: (g x -> h y) -> Nullable g x -> Nullable h y
mapNullable f (Eine g) = Eine (f g)
mapNullable _ Null = Null
{-# INLINE mapNullable #-}

-- | The inverse of 'inclusion'.
coinclusion :: (Include ys xs, Generate ys) => Nullable (Membership xs) :* ys
coinclusion = flip appEndo (htabulate (const Null))
  $ hfoldMap getConst'
  $ hmapWithIndex (\src dst -> Const' $ Endo $ pieceAt dst `over` const (Eine src))
  $ inclusion

-- | Extend a product and fill missing fields by 'Null'.
wrench :: (Generate ys, xs ⊆ ys) => h :* xs -> Nullable h :* ys
wrench xs = mapNullable (flip hlookup xs) `hmap` coinclusion
{-# INLINE wrench #-}

-- | Narrow the range of the sum, if possible.
retrench :: (Generate ys, xs ⊆ ys) => h :| ys -> Nullable ((:|) h) xs
retrench (EmbedAt i h) = views (pieceAt i) (mapNullable (`EmbedAt`h)) coinclusion
{-# INLINE retrench #-}
