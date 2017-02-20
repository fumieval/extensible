{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Inclusion
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
------------------------------------------------------------------------
module Data.Extensible.Inclusion (
  -- * Inclusion
#if __GLASGOW_HASKELL__ >= 800
  type (⊆)
#else
  (⊆)()
#endif
  , Include
  , inclusion
  , shrink
  , spread
  -- * Key-value
  , IncludeAssoc
  , Associated
  , inclusionAssoc
  , shrinkAssoc
  , spreadAssoc
  ) where

import Data.Constraint
import Data.Extensible.Class
import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig

-- | Unicode alias for 'Include'
type xs ⊆ ys = Include ys xs

-- | @ys@ contains @xs@
type Include ys = Forall (Member ys)

-- | Reify the inclusion of type level sets.
inclusion :: forall xs ys. Include ys xs => Membership ys :* xs
inclusion = hrepeatFor (Proxy :: Proxy (Member ys)) membership
{-# INLINABLE inclusion #-}

-- | /O(m log n)/ Select some elements.
shrink :: (xs ⊆ ys) => h :* ys -> h :* xs
shrink h = hmap (hindex h) inclusion
{-# INLINE shrink #-}

-- | /O(log n)/ Embed to a larger union.
spread :: (xs ⊆ ys) => h :| xs -> h :| ys
spread (EmbedAt i h) = views (pieceAt i) EmbedAt inclusion h
{-# INLINE spread #-}

------------------------------------------------------------------

type family Associated' (xs :: [Assoc k v]) (t :: Assoc k v) :: Constraint where
  Associated' xs (k ':> v) = Associate k v xs

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

-- | /O(m log n)/ Select some elements.
shrinkAssoc :: (IncludeAssoc ys xs) => h :* ys -> h :* xs
shrinkAssoc h = hmap (hindex h) inclusionAssoc
{-# INLINE shrinkAssoc #-}

-- | /O(log n)/ Embed to a larger union.
spreadAssoc :: (IncludeAssoc ys xs) => h :| xs -> h :| ys
spreadAssoc (EmbedAt i h) = views (pieceAt i) EmbedAt inclusionAssoc h
{-# INLINE spreadAssoc #-}
