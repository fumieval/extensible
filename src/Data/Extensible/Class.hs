{-# LANGUAGE MultiParamTypeClasses #-}
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
  , (∈)()
  , FindType
  -- * Association
  , Assoc(..)
  , Associate(..)
  , FindAssoc
  -- * Sugar
  , Elaborate
  , Elaborated(..)
  ) where
import Data.Extensible.Internal
import Data.Extensible.Wrapper
import Data.Profunctor

-- | This class allows us to use 'pieceAt' for both sums and products.
class (Functor f, Profunctor p) => Extensible f p q (t :: (k -> *) -> [k] -> *) where
  pieceAt :: Membership xs x -> p (h x) (f (h x)) -> q (t h xs) (f (t h xs))

-- | Accessor for an element.
piece :: (x ∈ xs, Extensible f p q t) => p (h x) (f (h x)) -> q (t h xs) (f (t h xs))
piece = pieceAt membership
{-# INLINE piece #-}

-- | Like 'piece', but reckon membership from its key.
pieceAssoc :: (Associate k v xs, Extensible f p q t) => p (h (k ':> v)) (f (h (k ':> v))) -> q (t h xs) (f (t h xs))
pieceAssoc = pieceAt association
{-# INLINE pieceAssoc #-}

itemAt :: (Wrapper h, Extensible f p q t) => Membership xs x -> p (Repr h x) (f (Repr h x)) -> q (t h xs) (f (t h xs))
itemAt m = pieceAt m . _Wrapper
{-# INLINE itemAt #-}

item :: (Wrapper h, Extensible f p q t, x ∈ xs) => proxy x -> p (Repr h x) (f (Repr h x)) -> q (t h xs) (f (t h xs))
item p = piece . _WrapperAs p
{-# INLINE item #-}

itemAssoc :: (Wrapper h, Extensible f p q t, Associate k v xs) => proxy k -> p (Repr h (k ':> v)) (f (Repr h (k ':> v))) -> q (t h xs) (f (t h xs))
itemAssoc p = pieceAssoc . _WrapperAs (proxyKey p)
{-# INLINE itemAssoc #-}

proxyKey :: proxy k -> Proxy (k ':> v)
proxyKey _ = Proxy
{-# INLINE proxyKey #-}
