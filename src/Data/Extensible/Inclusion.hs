{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Inclusion
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
------------------------------------------------------------------------
module Data.Extensible.Inclusion (
  -- * Inclusion
  type (⊆)
  , Include
  , inclusion
  , shrink
  , spread
  -- * Key-value
  , IncludeAssoc
  , Associated
  , Associated'
  , inclusionAssoc
  , shrinkAssoc
  , spreadAssoc
  ) where

import Data.Constraint
import Data.Extensible.Class
import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Extensible.Internal.Rig
import Data.Proxy

-- | Unicode alias for 'Include'
type xs ⊆ ys = Include ys xs

-- | @ys@ contains @xs@
type Include ys = Forall (Member ys)

-- | Reify the inclusion of type level sets.
inclusion :: forall xs ys. Include ys xs => Membership ys :* xs
inclusion = hrepeatFor (Proxy :: Proxy (Member ys)) membership
{-# INLINABLE inclusion #-}

-- | /O(n)/ Select some elements.
shrink :: (xs ⊆ ys) => h :* ys -> h :* xs
shrink h = hmap (hindex h) inclusion
{-# INLINE shrink #-}

-- | /O(1)/ Embed to a larger union.
spread :: (xs ⊆ ys) => h :| xs -> h :| ys
spread (EmbedAt i h) = views (pieceAt i) EmbedAt inclusion h
{-# INLINE spread #-}

------------------------------------------------------------------

type family Associated' (xs :: [Assoc k v]) (t :: Assoc k v) :: Constraint where
  Associated' xs (k ':> v) = Lookup xs k v

-- | @'Associated' xs (k ':> v)@ is equivalent to @'Associate' k v xs@
class Associated' xs t => Associated xs t where
  getAssociation :: Membership xs t

instance (Associated' xs t, t ~ (k ':> v)) => Associated xs t where
  getAssociation = association

-- | Similar to 'Include', but this focuses on keys.
type IncludeAssoc ys = Forall (Associated ys)

-- | Reify the inclusion of type level sets.
inclusionAssoc :: forall xs ys. IncludeAssoc ys xs => Membership ys :* xs
inclusionAssoc = hrepeatFor (Proxy :: Proxy (Associated ys)) getAssociation
{-# INLINABLE inclusionAssoc #-}

-- | /O(n)/ Select some elements.
shrinkAssoc :: (IncludeAssoc ys xs) => h :* ys -> h :* xs
shrinkAssoc h = hmap (hindex h) inclusionAssoc
{-# INLINE shrinkAssoc #-}

-- | /O(1)/ Embed to a larger union.
spreadAssoc :: (IncludeAssoc ys xs) => h :| xs -> h :| ys
spreadAssoc (EmbedAt i h) = views (pieceAt i) EmbedAt inclusionAssoc h
{-# INLINE spreadAssoc #-}
