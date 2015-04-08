{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Extensible.Class (
    Membership
  , mkMembership
  , runMembership
  , (∈)()
  , Member(..)
  , remember
  , Expecting
  , Missing
  , Ambiguous
  , Assoc(..)
  , Associate(..)
  , Extensible(..)
  , piece
  , pieceAssoc
  ) where
import Data.Extensible.Internal

class Extensible f p q (t :: (k -> *) -> [k] -> *) where
  pieceAt :: Membership xs x -> p (h x) (f (h x)) -> q (t h xs) (f (t h xs))

piece :: (x ∈ xs, Extensible f p q t) => p (h x) (f (h x)) -> q (t h xs) (f (t h xs))
piece = pieceAt membership
{-# INLINE piece #-}

-- | /O(log n)/ A lens for a specific element.
pieceAssoc :: (Associate k v xs, Extensible f p q t) => p (h (k ':> v)) (f (h (k ':> v))) -> q (t h xs) (f (t h xs))
pieceAssoc = pieceAt association
{-# INLINE pieceAssoc #-}
