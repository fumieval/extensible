{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Extensible.Class (
  -- * Class
   Extensible(..)
  , piece
  , pieceAssoc
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

-- | This class allows us to use 'pieceAt' for both sums and products.
class Extensible f p q (t :: (k -> *) -> [k] -> *) where
  pieceAt :: Membership xs x -> p (h x) (f (h x)) -> q (t h xs) (f (t h xs))

-- | Accessor for an element.
piece :: (x ∈ xs, Extensible f p q t) => p (h x) (f (h x)) -> q (t h xs) (f (t h xs))
piece = pieceAt membership
{-# INLINE piece #-}

-- | Like 'piece', but reckon membership from its key.
pieceAssoc :: (Associate k v xs, Extensible f p q t) => p (h (k ':> v)) (f (h (k ':> v))) -> q (t h xs) (f (t h xs))
pieceAssoc = pieceAt association
{-# INLINE pieceAssoc #-}
