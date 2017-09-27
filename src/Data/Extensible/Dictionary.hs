{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------
--
-- Module      :  Data.Extensible.Dictionary
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Reification of constraints using extensible data types.
-- Also includes orphan instances.
-----------------------------------------------------------------------
module Data.Extensible.Dictionary (library, WrapForall, Instance1) where
import Control.DeepSeq
import Data.Extensible.Class
import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Constraint
import Data.Extensible.Struct
import Data.Extensible.Wrapper
import Data.Semigroup
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- | Reify a collection of dictionaries, as you wish.
library :: forall c xs. Forall c xs => Comp Dict c :* xs
library = hrepeatFor (Proxy :: Proxy c) $ Comp Dict
{-# INLINE library #-}

instance WrapForall Show h xs => Show (h :* xs) where
  showsPrec d xs = showParen (d > 0)
    $ henumerateFor (Proxy :: Proxy (Instance1 Show h)) xs
    (\i r -> showsPrec 0 (hlookup i xs) . showString " <: " . r)
    (showString "nil")

instance WrapForall Eq h xs => Eq (h :* xs) where
  xs == ys = henumerateFor (Proxy :: Proxy (Instance1 Eq h)) xs
    (\i r -> hlookup i xs == hlookup i ys && r) True
  {-# INLINE (==) #-}

instance (Eq (h :* xs), WrapForall Ord h xs) => Ord (h :* xs) where
  compare xs ys = henumerateFor (Proxy :: Proxy (Instance1 Ord h)) xs
    (\i r -> (hlookup i xs `compare` hlookup i ys) `mappend` r) mempty
  {-# INLINE compare #-}

instance WrapForall Semigroup h xs => Semigroup (h :* xs) where
  (<>) = hzipWith3 (\(Comp Dict) -> (<>))
    (library :: Comp Dict (Instance1 Semigroup h) :* xs)
  {-# INLINE (<>) #-}

instance WrapForall Monoid h xs => Monoid (h :* xs) where
  mempty = hrepeatFor (Proxy :: Proxy (Instance1 Monoid h)) mempty
  {-# INLINE mempty #-}
  mappend = hzipWith3 (\(Comp Dict) -> mappend)
    (library :: Comp Dict (Instance1 Monoid h) :* xs)
  {-# INLINE mappend #-}

instance WrapForall Bounded h xs => Bounded (h :* xs) where
  minBound = hrepeatFor (Proxy :: Proxy (Instance1 Bounded h)) minBound
  maxBound = hrepeatFor (Proxy :: Proxy (Instance1 Bounded h)) maxBound

instance WrapForall Arbitrary h xs => Arbitrary (h :* xs) where
  arbitrary = hgenerateFor (Proxy :: Proxy (Instance1 Arbitrary h)) (const arbitrary)
  shrink xs = henumerateFor (Proxy :: Proxy (Instance1 Arbitrary h))
    (Proxy :: Proxy xs) (\i -> (++)
    $ map (\x -> hmodify (\s -> set s i x) xs) $ shrink $ hindex xs i)
    []

instance WrapForall NFData h xs => NFData (h :* xs) where
  rnf xs = henumerateFor (Proxy :: Proxy (Instance1 NFData h)) (Proxy :: Proxy xs)
    (\i -> deepseq (hlookup i xs)) ()
  {-# INLINE rnf #-}

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

instance WrapForall NFData h xs => NFData (h :| xs) where
  rnf (EmbedAt i h) = views (pieceAt i) (\(Comp Dict) -> rnf h) (library :: Comp Dict (Instance1 NFData h) :* xs)
  {-# INLINE rnf #-}

instance WrapForall Arbitrary h xs => Arbitrary (h :| xs) where
  arbitrary = choose (0, hcount (Proxy :: Proxy xs)) >>= henumerateFor
      (Proxy :: Proxy (Instance1 Arbitrary h))
      (Proxy :: Proxy xs)
      (\m r i -> if i == 0
        then EmbedAt m <$> arbitrary
        else r (i - 1))
        (error "Impossible")
  shrink (EmbedAt i h) = views (pieceAt i)
    (\(Comp Dict) -> EmbedAt i <$> shrink h)
    (library :: Comp Dict (Instance1 Arbitrary h) :* xs)

-- | Forall upon a wrapper
type WrapForall c h = Forall (Instance1 c h)

-- | Composition for a class and a wrapper
class c (h x) => Instance1 c h x
instance c (h x) => Instance1 c h x
