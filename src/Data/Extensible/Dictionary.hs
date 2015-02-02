{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
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

type DictOf c g h = forall xs. WrapForall c g xs => h :* xs

dictShow :: forall h. DictOf Show h (Match h (Int -> ShowS))
dictShow = generateFor (Proxy :: Proxy (Instance1 Show h)) $ const $ Match (flip showsPrec)

dictEq :: forall h. DictOf Eq h (Wrap2 h Bool)
dictEq = generateFor (Proxy :: Proxy (Instance1 Eq h)) $ const $ Wrap2 (==)

dictOrd :: forall h. DictOf Ord h (Wrap2 h Ordering)
dictOrd = generateFor (Proxy :: Proxy (Instance1 Ord h)) $ const $ Wrap2 compare

data WrapMonoid h x = WrapMonoid { unwrapEmpty :: h x, unwrapAppend :: h x -> h x -> h x }

dictMonoid :: forall h. DictOf Monoid h (WrapMonoid h)
dictMonoid = generateFor (Proxy :: Proxy (Instance1 Monoid h)) $ const $ WrapMonoid mempty mappend

instance WrapForall Show h xs => Show (h :* xs) where
  showsPrec d = showParen (d > 0)
    . (.showString "Nil")
    . foldr (.) id
    . getMerged
    . hfoldMap getConst'
    . hzipWith (\(Match f) h -> Const' $ MergeList [f h 0 . showString " <: "]) dictShow

instance WrapForall Eq h xs => Eq (h :* xs) where
  xs == ys = getAll $ hfoldMap (All . getConst')
    $ hzipWith3 (\(Wrap2 f) x y -> Const' (f x y)) dictEq xs ys
  {-# INLINE (==) #-}

instance (Eq (h :* xs), WrapForall Ord h xs) => Ord (h :* xs) where
  compare xs ys = hfoldMap getConst'
    $ hzipWith3 (\(Wrap2 f) x y -> Const' (f x y)) dictOrd xs ys
  {-# INLINE compare #-}

instance WrapForall Monoid h xs => Monoid (h :* xs) where
  mempty = hmap unwrapEmpty dictMonoid
  {-# INLINE mempty #-}
  mappend xs ys = hzipWith3 unwrapAppend dictMonoid xs ys
  {-# INLINE mappend #-}

instance WrapForall Show h xs => Show (h :| xs) where
  showsPrec d (UnionAt pos h) = showParen (d > 10) $ showString "embed "
    . runMatch (hlookup pos dictShow) h 11

instance WrapForall Eq h xs => Eq (h :| xs) where
  UnionAt p g == UnionAt q h = case compareMembership p q of
    Left _ -> False
    Right Refl -> unwrap2 (hlookup p dictEq) g h
  {-# INLINE (==) #-}

instance (Eq (h :| xs), WrapForall Ord h xs) => Ord (h :| xs) where
  UnionAt p g `compare` UnionAt q h = case compareMembership p q of
    Left x -> x
    Right Refl -> unwrap2 (hlookup p dictOrd) g h
  {-# INLINE compare #-}

type WrapForall c h = Forall (Instance1 c h)

-- | Composition for a class and a wrapper
class c (h x) => Instance1 c h x
instance c (h x) => Instance1 c h x
