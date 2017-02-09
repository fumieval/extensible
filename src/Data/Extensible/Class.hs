{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables #-}
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
  -- * Generation
  , Generate(..)
  , Forall(..)
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

-- | Access a specified element through a wrapper.
itemAt :: (Wrapper h, Extensible f p t) => Membership xs x -> Optic' p f (t h xs) (Repr h x)
itemAt m = pieceAt m . _Wrapper
{-# INLINE itemAt #-}

-- | Access an element through a wrapper.
item :: (Wrapper h, Extensible f p t, x ∈ xs) => proxy x -> Optic' p f (t h xs) (Repr h x)
item p = piece . _WrapperAs p
{-# INLINE item #-}

-- | Access an element specified by the key type through a wrapper.
itemAssoc :: (Wrapper h, Extensible f p t, Associate k v xs)
  => proxy k -> Optic' p f (t h xs) (Repr h (k ':> v))
itemAssoc p = pieceAssoc . _WrapperAs (proxyKey p)
{-# INLINE itemAssoc #-}

proxyKey :: proxy k -> Proxy (k ':> v)
proxyKey _ = Proxy
{-# INLINE proxyKey #-}

-- | Given a function that maps types to values, we can "collect" entities all you want.
class Generate (xs :: [k]) where
  -- | /O(n)/ Generate a product with the given function.
  henumerate :: (forall x. Membership xs x -> r -> r) -> r -> r

instance Generate '[] where
  henumerate _ r = r
  {-# INLINE henumerate #-}

instance Generate xs => Generate (x ': xs) where
  henumerate f r = f here $ henumerate (f . navNext) r
  {-# INLINE henumerate #-}

-- | Given a function that maps types to values, we can "collect" entities all you want.
class Generate xs => Forall c (xs :: [k]) where
  -- | /O(n)/ Generate a product with the given function.
  henumerateFor :: proxy c -> proxy' xs -> (forall x. c x => Membership xs x -> r -> r) -> r -> r

instance Forall c '[] where
  henumerateFor _ _ _ r = r
  {-# INLINE henumerateFor #-}

instance (c x, Forall c xs) => Forall c (x ': xs) where
  henumerateFor p _ f r = f here $ henumerateFor p (Proxy :: Proxy xs) (f . navNext) r
  {-# INLINE henumerateFor #-}
