{-# LANGUAGE ScopedTypeVariables #-}
module Data.Extensible.HList where

import Prelude hiding (length)
import Data.Extensible.Internal

data HList (h :: k -> *) (xs :: [k]) where
  HNil :: HList h '[]
  HCons :: h x -> HList h xs -> HList h (x ': xs)

infixr 5 `HCons`

htraverse :: Applicative f => (forall x. g x -> f (h x)) -> HList g xs -> f (HList h xs)
htraverse _ HNil = pure HNil
htraverse f (HCons h xs) = HCons <$> f h <*> htraverse f xs

htraverseWithIndex :: forall f g h xs. Applicative f
    => (forall x. Membership xs x -> g x -> f (h x)) -> HList g xs -> f (HList h xs)
htraverseWithIndex f = go id where
  go :: (forall x. Membership t x -> Membership xs x) -> HList g t -> f (HList h t)
  go k (HCons x xs) = HCons <$> f (k here) x <*> go (k . navNext) xs
  go _ HNil = pure HNil

hfoldrWithIndex :: forall h r xs. (forall x. Membership xs x -> h x -> r -> r) -> r -> HList h xs -> r
hfoldrWithIndex f r = go id where
  go :: (forall x. Membership t x -> Membership xs x) -> HList h t -> r
  go k (HCons x xs) = f (k here) x $ go (k . navNext) xs
  go _ HNil = r

length :: HList h xs -> Int
length HNil = 0
length (HCons _ xs) = length xs + 1
