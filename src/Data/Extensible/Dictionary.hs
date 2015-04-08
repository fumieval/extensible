{-# LANGUAGE PolyKinds, TypeFamilies, InstanceSigs, UndecidableInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------
--
-- Module      :  Data.Extensible.Dictionary
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Reifying some classes to make instances for (':*') and (':|')
-----------------------------------------------------------------------
module Data.Extensible.Dictionary where
import Data.Monoid
import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Constraint

library :: forall c xs. Forall c xs => Comp Dict c :* xs
library = htabulateFor (Proxy :: Proxy c) $ const (Comp Dict)
{-# INLINE library #-}

instance WrapForall Show h xs => Show (h :* xs) where
  showsPrec d = showParen (d > 0)
    . (.showString "Nil")
    . foldr (.) id
    . getMerged
    . hfoldMap getConst'
    . hzipWith (\(Comp Dict) h -> Const' $ MergeList [showsPrec 0 h . showString " <: "]) (library :: Comp Dict (Instance1 Show h) :* xs)

instance WrapForall Eq h xs => Eq (h :* xs) where
  xs == ys = getAll $ hfoldMap (All . getConst')
    $ hzipWith3 (\(Comp Dict) x y -> Const' $ x == y) (library :: Comp Dict (Instance1 Eq h) :* xs) xs ys
  {-# INLINE (==) #-}

instance (Eq (h :* xs), WrapForall Ord h xs) => Ord (h :* xs) where
  compare xs ys = hfoldMap getConst'
    $ hzipWith3 (\(Comp Dict) x y -> Const' $ compare x y) (library :: Comp Dict (Instance1 Ord h) :* xs) xs ys
  {-# INLINE compare #-}

instance WrapForall Monoid h xs => Monoid (h :* xs) where
  mempty = hmap (\(Comp Dict) -> mempty) (library :: Comp Dict (Instance1 Monoid h) :* xs)
  {-# INLINE mempty #-}
  mappend xs ys = hzipWith3 (\(Comp Dict) -> mappend) (library :: Comp Dict (Instance1 Monoid h) :* xs) xs ys
  {-# INLINE mappend #-}

instance WrapForall Show h xs => Show (h :| xs) where
  showsPrec d (EmbedAt i h) = showParen (d > 10) $ showString "EmbedAt "
    . showsPrec 11 i
    . showString " "
    . views (pieceAt i) (\(Comp Dict) -> showsPrec 11 h) (library :: Comp Dict (Instance1 Show h) :* xs)

instance WrapForall Eq h xs => Eq (h :| xs) where
  EmbedAt p g == EmbedAt q h = case compareMembership p q of
    Left _ -> False
    Right Refl -> views (pieceAt p) (\(Comp Dict) -> g == h) (library :: Comp Dict (Instance1 Eq h) :* xs)
  {-# INLINE (==) #-}

instance (Eq (h :| xs), WrapForall Ord h xs) => Ord (h :| xs) where
  EmbedAt p g `compare` EmbedAt q h = case compareMembership p q of
    Left x -> x
    Right Refl -> views (pieceAt p) (\(Comp Dict) -> compare g h) (library :: Comp Dict (Instance1 Ord h) :* xs)
  {-# INLINE compare #-}

-- | Forall upon a wrapper
type WrapForall c h = Forall (Instance1 c h)

-- | Composition for a class and a wrapper
class c (h x) => Instance1 c h x
instance c (h x) => Instance1 c h x
