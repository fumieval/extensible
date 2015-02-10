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

class Reifiable c where
  data Dictionary c (h :: k -> *) (x :: k)
  library :: WrapForall c h xs => Dictionary c h :* xs

type DictOf c g h = forall xs. WrapForall c g xs => h :* xs

instance Reifiable Show where
  data Dictionary Show h x = DictShow { getShowsPrec :: Int -> h x -> ShowS }
  library :: forall h xs. WrapForall Show h xs => Dictionary Show h :* xs
  library = generateFor (Proxy :: Proxy (Instance1 Show h)) $ const $ DictShow showsPrec

instance Reifiable Eq where
  data Dictionary Eq h x = DictEq { getEq :: h x -> h x -> Bool }
  library :: forall h xs. WrapForall Eq h xs => Dictionary Eq h :* xs
  library = generateFor (Proxy :: Proxy (Instance1 Eq h)) $ const $ DictEq (==)

instance Reifiable Ord where
  data Dictionary Ord h x = DictOrd { getCompare :: h x -> h x -> Ordering }
  library :: forall h xs. WrapForall Ord h xs => Dictionary Ord h :* xs
  library = generateFor (Proxy :: Proxy (Instance1 Ord h)) $ const $ DictOrd compare

instance Reifiable Monoid where
  data Dictionary Monoid h x = DictMonoid { getMempty :: h x, getMappend :: h x -> h x -> h x }
  library :: forall h xs. WrapForall Monoid h xs => Dictionary Monoid h :* xs
  library = generateFor (Proxy :: Proxy (Instance1 Monoid h)) $ const $ DictMonoid mempty mappend

instance Reifiable B.Binary where
  data Dictionary B.Binary h x = DictBinary { getGet :: B.Get (h x), getPut :: h x -> B.Put }
  library :: forall h xs. WrapForall B.Binary h xs => Dictionary B.Binary h :* xs
  library = generateFor (Proxy :: Proxy (Instance1 B.Binary h)) $ const $ DictBinary B.get B.put

instance WrapForall Show h xs => Show (h :* xs) where
  showsPrec d = showParen (d > 0)
    . (.showString "Nil")
    . foldr (.) id
    . getMerged
    . hfoldMap getConst'
    . hzipWith (\f h -> Const' $ MergeList [getShowsPrec f 0 h . showString " <: "]) library

instance WrapForall Eq h xs => Eq (h :* xs) where
  xs == ys = getAll $ hfoldMap (All . getConst')
    $ hzipWith3 (\f x y -> Const' $ getEq f x y) library xs ys
  {-# INLINE (==) #-}

instance (Eq (h :* xs), WrapForall Ord h xs) => Ord (h :* xs) where
  compare xs ys = hfoldMap getConst'
    $ hzipWith3 (\f x y -> Const' $ getCompare f x y) library xs ys
  {-# INLINE compare #-}

instance WrapForall Monoid h xs => Monoid (h :* xs) where
  mempty = hmap getMempty library
  {-# INLINE mempty #-}
  mappend xs ys = hzipWith3 getMappend library xs ys
  {-# INLINE mappend #-}

instance WrapForall B.Binary h xs => B.Binary (h :* xs) where
  get = generateForA (Proxy :: Proxy (Instance1 B.Binary h)) (const B.get)
  put = flip appEndo (return ()) . hfoldMap getConst' . hzipWith (\dic x -> Const' $ Endo $ (getPut dic x >>)) library

instance WrapForall Show h xs => Show (h :| xs) where
  showsPrec d (UnionAt pos h) = showParen (d > 10) $ showString "embed "
    . getShowsPrec (hlookup pos library) 11 h

instance WrapForall Eq h xs => Eq (h :| xs) where
  UnionAt p g == UnionAt q h = case compareMembership p q of
    Left _ -> False
    Right Refl -> views (sectorAt p) getEq library g h
  {-# INLINE (==) #-}

instance (Eq (h :| xs), WrapForall Ord h xs) => Ord (h :| xs) where
  UnionAt p g `compare` UnionAt q h = case compareMembership p q of
    Left x -> x
    Right Refl -> views (sectorAt p) getCompare library g h
  {-# INLINE compare #-}

type WrapForall c h = Forall (Instance1 c h)

-- | Composition for a class and a wrapper
class c (h x) => Instance1 c h x
instance c (h x) => Instance1 c h x
