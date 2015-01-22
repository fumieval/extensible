{-# LANGUAGE PolyKinds, ScopedTypeVariables #-}
module Data.Extensible.Internal.HList where

import Data.Extensible.Internal
import Data.Extensible.Product
import Data.Proxy

data HList (h :: k -> *) (s :: [k]) where
  HNil :: HList h '[]
  HCons :: h x -> HList h xs -> HList h (x ': xs)

infixr 5 `HCons`

merge :: HList h xs -> HList h ys -> HList h (Merge xs ys)
merge (HCons x xs) (HCons y ys) = x `HCons` y `HCons` merge xs ys
merge HNil ys = ys
merge xs HNil = xs

split :: forall h xs. HList h xs -> (HList h (Half xs), HList h (Half (Tail xs)))
split (HCons x (HCons y ys)) = let (a, b) = split ys
  in (HCons x a, lemmaHalfTail (Proxy :: Proxy (Tail (Tail xs))) $ HCons y b)
split (HCons x HNil) = (HCons x HNil, HNil)
split HNil = (HNil, HNil)

fromProduct :: h :* xs -> HList h xs
fromProduct (Tree h a b) = HCons h $ lemmaMerging $ fromProduct a `merge` fromProduct b
fromProduct Nil = HNil

toProduct :: HList h xs -> h :* xs
toProduct (HCons x xs) = let (a, b) = split xs in Tree x (toProduct a) (toProduct b)
toProduct HNil = Nil
