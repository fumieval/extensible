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
import qualified Data.Binary as B

library :: forall c h xs. WrapForall c h xs => Dictionary c h :* xs
library = htabulateFor (Proxy :: Proxy (Instance1 c h)) $ const Dictionary

data Dictionary c h x where
  Dictionary :: c (h x) => Dictionary c h x

instance WrapForall Show h xs => Show (h :* xs) where
  showsPrec d = showParen (d > 0)
    . (.showString "Nil")
    . foldr (.) id
    . getMerged
    . hfoldMap getConst'
    . hzipWith (\Dictionary h -> Const' $ MergeList [showsPrec 0 h . showString " <: "]) (library :: Dictionary Show h :* xs)

instance WrapForall Eq h xs => Eq (h :* xs) where
  xs == ys = getAll $ hfoldMap (All . getConst')
    $ hzipWith3 (\Dictionary x y -> Const' $ x == y) (library :: Dictionary Eq h :* xs) xs ys
  {-# INLINE (==) #-}

instance (Eq (h :* xs), WrapForall Ord h xs) => Ord (h :* xs) where
  compare xs ys = hfoldMap getConst'
    $ hzipWith3 (\Dictionary x y -> Const' $ compare x y) (library :: Dictionary Ord h :* xs) xs ys
  {-# INLINE compare #-}

instance WrapForall Monoid h xs => Monoid (h :* xs) where
  mempty = hmap (\Dictionary -> mempty) (library :: Dictionary Monoid h :* xs)
  {-# INLINE mempty #-}
  mappend xs ys = hzipWith3 (\Dictionary -> mappend) (library :: Dictionary Monoid h :* xs) xs ys
  {-# INLINE mappend #-}

instance WrapForall B.Binary h xs => B.Binary (h :* xs) where
  get = hgenerateFor (Proxy :: Proxy (Instance1 B.Binary h)) (const B.get)
  put = flip appEndo (return ()) . hfoldMap getConst' . hzipWith (\Dictionary x -> Const' $ Endo $ (B.put x >>)) (library :: Dictionary B.Binary h :* xs)

instance WrapForall Show h xs => Show (h :| xs) where
  showsPrec d (UnionAt pos h) = showParen (d > 10) $ showString "embed "
    . views (sectorAt pos) (\Dictionary -> showsPrec 11 h) (library :: Dictionary Show h :* xs)

instance WrapForall Eq h xs => Eq (h :| xs) where
  UnionAt p g == UnionAt q h = case compareMembership p q of
    Left _ -> False
    Right Refl -> views (sectorAt p) (\Dictionary -> g == h) (library :: Dictionary Eq h :* xs)
  {-# INLINE (==) #-}

instance (Eq (h :| xs), WrapForall Ord h xs) => Ord (h :| xs) where
  UnionAt p g `compare` UnionAt q h = case compareMembership p q of
    Left x -> x
    Right Refl -> views (sectorAt p) (\Dictionary -> compare g h) (library :: Dictionary Ord h :* xs)
  {-# INLINE compare #-}

-- | Forall upon a wrapper
type WrapForall c h = Forall (Instance1 c h)

-- | Composition for a class and a wrapper
class c (h x) => Instance1 c h x
instance c (h x) => Instance1 c h x
