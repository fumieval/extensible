{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Class
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  MPTCs
--
-----------------------------------------------------------------------------
module Data.Extensible.Class (
  -- * Class
   Extensible(..)
  , piece
  , pieceAssoc
  , itemAt
  , item
  , itemAssoc
  -- * Membership
  , Membership
  , mkMembership
  -- * Member
  , Member(..)
  , remember
#if __GLASGOW_HASKELL__ >= 800
  , type (∈)
#else
  , (∈)()
#endif
  , FindType
  -- * Association
  , Assoc(..)
#if __GLASGOW_HASKELL__ >= 800
  , type (>:)
#else
  , (>:)()
#endif
  , Associate(..)
  , FindAssoc
  -- * Sugar
  , Elaborate
  , Elaborated(..)
  ) where
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig (Optic')
import Data.Extensible.Wrapper
import Data.Profunctor

-- | This class allows us to use 'pieceAt' for both sums and products.
class (Functor f, Profunctor p) => Extensible f p (t :: (k -> *) -> [k] -> *) where
  pieceAt :: Membership xs x -> Optic' p f (t h xs) (h x)

-- | Accessor for an element.
piece :: (x ∈ xs, Extensible f p t) => Optic' p f (t h xs) (h x)
piece = pieceAt membership
{-# INLINE piece #-}

-- | Like 'piece', but reckon membership from its key.
pieceAssoc :: (Associate k v xs, Extensible f p t) => Optic' p f (t h xs) (h (k ':> v))
pieceAssoc = pieceAt association
{-# INLINE pieceAssoc #-}

itemAt :: (Wrapper h, Extensible f p t) => Membership xs x -> Optic' p f (t h xs) (Repr h x)
itemAt m = pieceAt m . _Wrapper
{-# INLINE itemAt #-}

item :: (Wrapper h, Extensible f p t, x ∈ xs) => proxy x -> Optic' p f (t h xs) (Repr h x)
item p = piece . _WrapperAs p
{-# INLINE item #-}

itemAssoc :: (Wrapper h, Extensible f p t, Associate k v xs)
  => proxy k -> Optic' p f (t h xs) (Repr h (k ':> v))
itemAssoc p = pieceAssoc . _WrapperAs (proxyKey p)
{-# INLINE itemAssoc #-}

proxyKey :: proxy k -> Proxy (k ':> v)
proxyKey _ = Proxy
{-# INLINE proxyKey #-}
