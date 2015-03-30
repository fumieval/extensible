{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , remember
  , Expecting
  , Missing
  , Ambiguous
  , ord
  , Assoc(..)
  , Associate(..)
  -- * Inclusion
  , (⊆)()
  , Include
  , inclusion
  , shrink
  , spread
  -- * Dictionary-like
  , IncludeAssoc
  , Associated
  , inclusionAssoc
  , shrinkAssoc
  , spreadAssoc
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
inclusion = htabulateFor (Proxy :: Proxy (Member ys)) (const membership)

-- | /O(m log n)/ Select some elements.
shrink :: (xs ⊆ ys) => h :* ys -> h :* xs
shrink h = hmap (`hlookup` h) inclusion
{-# INLINE shrink #-}

-- | /O(log n)/ Embed to a larger union.
spread :: (xs ⊆ ys) => h :| xs -> h :| ys
spread (UnionAt pos h) = views (sectorAt pos) UnionAt inclusion h
{-# INLINE spread #-}

-- | The inverse of 'inclusion'.
coinclusion :: (Include ys xs, Generate ys) => Nullable (Membership xs) :* ys
coinclusion = flip appEndo (htabulate (const Null))
  $ hfoldMap getConst'
  $ hmapWithIndex (\src dst -> Const' $ Endo $ sectorAt dst `over` const (Eine src))
  $ inclusion

-- | Extend a product and fill missing fields by 'Null'.
wrench :: (Generate ys, xs ⊆ ys) => h :* xs -> Nullable h :* ys
wrench xs = mapNullable (flip hlookup xs) `hmap` coinclusion
{-# INLINE wrench #-}

-- | Narrow the range of the sum, if possible.
retrench :: (Generate ys, xs ⊆ ys) => h :| ys -> Nullable ((:|) h) xs
retrench (UnionAt pos h) = views (sectorAt pos) (mapNullable (`UnionAt`h)) coinclusion
{-# INLINE retrench #-}

------------------------------------------------------------------

class Associated xs t where
  getAssociation :: Membership xs t

instance Associate k v xs => Associated xs (k ':> v) where
  getAssociation = association

-- | Similar to 'Include', but works nicely for key-value pairs.
type IncludeAssoc ys xs = Forall (Associated ys) xs

-- | Reify the inclusion of type level sets.
inclusionAssoc :: forall xs ys. IncludeAssoc ys xs => Membership ys :* xs
inclusionAssoc = htabulateFor (Proxy :: Proxy (Associated ys)) (const getAssociation)

-- | /O(m log n)/ Select some elements.
shrinkAssoc :: (IncludeAssoc ys xs) => h :* ys -> h :* xs
shrinkAssoc h = hmap (`hlookup` h) inclusionAssoc
{-# INLINE shrinkAssoc #-}

-- | /O(log n)/ Embed to a larger union.
spreadAssoc :: (IncludeAssoc ys xs) => h :| xs -> h :| ys
spreadAssoc (UnionAt pos h) = views (sectorAt pos) UnionAt inclusionAssoc h
{-# INLINE spreadAssoc #-}
