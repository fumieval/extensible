{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Inclusion
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
------------------------------------------------------------------------
module Data.Extensible.Inclusion (
  -- * Membership
    Membership
  , runMembership
  , (∈)()
  , Member(..)
  , Expecting
  , Missing
  , Ambiguous
  , ord
  -- * Inclusion
  , (⊆)()
  , Include
  , inclusion
  , shrink
  , subset
  , spread
  -- * Inverse
  , coinclusion
  , wrench
  , retrench
  , Nullable(..)
  , nullable
  , mapNullable
  ) where

import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Monoid

-- | Unicode alias for 'Include'
type xs ⊆ ys = Include ys xs

-- | @ys@ contains @xs@
type Include ys = Forall (Member ys)

-- | Reify the inclusion of type level sets.
inclusion :: forall xs ys. Include ys xs => Membership ys :* xs
inclusion = generateFor (Proxy :: Proxy (Member ys)) (const membership)

-- | /O(m log n)/ Select some elements.
shrink :: (xs ⊆ ys) => h :* ys -> h :* xs
shrink h = hmap (\pos -> hlookup pos h) inclusion
{-# INLINE shrink #-}

subset :: (xs ⊆ ys) => Lens' (h :* ys) (h :* xs)
subset f ys = fmap (write ys) $ f (shrink ys) where
  write y xs = flip appEndo y
    $ hfoldMap getConst'
    $ hzipWith (\dst src -> Const' $ Endo $ sectorAt dst `over` const src) inclusion xs

-- | /O(log n)/ Embed to a larger union.
spread :: (xs ⊆ ys) => h :| xs -> h :| ys
spread (UnionAt pos h) = UnionAt (hlookup pos inclusion) h
{-# INLINE spread #-}

-- | The inverse of 'inclusion'.
coinclusion :: (Include ys xs, Generate ys) => Nullable (Membership xs) :* ys
coinclusion = flip appEndo (generate (const Null))
  $ hfoldMap getConst'
  $ htabulate (\src dst -> Const' $ Endo $ sectorAt dst `over` const (Eine src))
  $ inclusion

-- | Extend a product and fill missing fields by 'Null'.
wrench :: (Generate ys, xs ⊆ ys) => h :* xs -> Nullable h :* ys
wrench xs = mapNullable (flip hlookup xs) `hmap` coinclusion
{-# INLINE wrench #-}

-- | Narrow the range of the sum, if possible.
retrench :: (Generate ys, xs ⊆ ys) => h :| ys -> Nullable ((:|) h) xs
retrench (UnionAt pos h) = flip UnionAt h `mapNullable` hlookup pos coinclusion
{-# INLINE retrench #-}
