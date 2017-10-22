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
import Data.Functor.Identity
import Data.Hashable
import Data.Semigroup
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U
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

instance WrapForall Hashable h xs => Hashable (h :* xs) where
  hashWithSalt = hfoldlWithIndexFor (Proxy :: Proxy (Instance1 Hashable h))
    (const hashWithSalt)
  {-# INLINE hashWithSalt #-}

instance WrapForall Bounded h xs => Bounded (h :* xs) where
  minBound = hrepeatFor (Proxy :: Proxy (Instance1 Bounded h)) minBound
  maxBound = hrepeatFor (Proxy :: Proxy (Instance1 Bounded h)) maxBound

newtype instance U.MVector s (h :* xs) = MV_Product (Comp (U.MVector s) h :* xs)
newtype instance U.Vector (h :* xs) = V_Product (Comp U.Vector h :* xs)

#define ENUM_EACH(expr) henumerateFor \
  (Proxy :: Proxy (Instance1 U.Unbox h)) (Proxy :: Proxy (x ': xs)) \
  ((>>) . (expr)) (return ())

hlookupC :: Membership xs a -> Comp f g :* xs -> f (g a)
hlookupC i = getComp . hlookup i

instance WrapForall U.Unbox h (x ': xs) => G.Vector U.Vector (h :* (x ': xs)) where
  basicUnsafeFreeze (MV_Product v) = fmap V_Product
    $ hgenerateFor (Proxy :: Proxy (Instance1 U.Unbox h))
    $ \m -> Comp <$> G.basicUnsafeFreeze (hlookupC m v)
  basicUnsafeThaw (V_Product v) = fmap MV_Product
    $ hgenerateFor (Proxy :: Proxy (Instance1 U.Unbox h))
    $ \m -> Comp <$> G.basicUnsafeThaw (hlookupC m v)
  basicLength (V_Product v) = G.basicLength $ getComp $ hindex v here
  basicUnsafeSlice i n (V_Product v) = V_Product
    $ htabulateFor (Proxy :: Proxy (Instance1 U.Unbox h))
    $ \m -> Comp $ G.basicUnsafeSlice i n (hlookupC m v)
  basicUnsafeIndexM (V_Product v) i = hgenerateFor (Proxy :: Proxy (Instance1 U.Unbox h))
    $ \m -> G.basicUnsafeIndexM (hlookupC m v) i
  basicUnsafeCopy (MV_Product v) (V_Product w)
    = ENUM_EACH(\i -> G.basicUnsafeCopy (hlookupC i v) (hlookupC i w))

instance WrapForall U.Unbox h (x ': xs) => M.MVector U.MVector (h :* (x ': xs)) where
  basicLength (MV_Product v) = M.basicLength $ getComp $ hindex v here
  basicUnsafeSlice i n (MV_Product v) = MV_Product
    $ htabulateFor (Proxy :: Proxy (Instance1 U.Unbox h))
    $ \m -> Comp $ M.basicUnsafeSlice i n (hlookupC m v)
  basicOverlaps (MV_Product v1) (MV_Product v2) = henumerateFor
    (Proxy :: Proxy (Instance1 U.Unbox h)) (Proxy :: Proxy (x ': xs))
    (\i -> (||) $ M.basicOverlaps (hlookupC i v1) (hlookupC i v2))
    False
  basicUnsafeNew n = fmap MV_Product
    $ hgenerateFor (Proxy :: Proxy (Instance1 U.Unbox h))
    (const $ Comp <$> M.basicUnsafeNew n)
  basicInitialize (MV_Product v) = ENUM_EACH(\i -> M.basicInitialize $ hlookupC i v)
  basicUnsafeReplicate n x = fmap MV_Product
    $ hgenerateFor (Proxy :: Proxy (Instance1 U.Unbox h))
    $ \m -> fmap Comp $ M.basicUnsafeReplicate n $ hlookup m x
  basicUnsafeRead (MV_Product v) i = hgenerateFor (Proxy :: Proxy (Instance1 U.Unbox h))
    (\m -> M.basicUnsafeRead (hlookupC m v) i)
  basicUnsafeWrite (MV_Product v) i x = ENUM_EACH(\m -> M.basicUnsafeWrite (hlookupC m v) i (hlookup m x))
  basicClear (MV_Product v) = ENUM_EACH(\i -> M.basicClear $ hlookupC i v)
  basicSet (MV_Product v) x = ENUM_EACH(\i -> M.basicSet (hlookupC i v) (hlookup i x))
  basicUnsafeCopy (MV_Product v1) (MV_Product v2)
    = ENUM_EACH(\i -> M.basicUnsafeCopy (hlookupC i v1) (hlookupC i v2))
  basicUnsafeMove (MV_Product v1) (MV_Product v2)
    = ENUM_EACH(\i -> M.basicUnsafeMove (hlookupC i v1) (hlookupC i v2))
  basicUnsafeGrow (MV_Product v) n = fmap MV_Product
    $ hgenerateFor (Proxy :: Proxy (Instance1 U.Unbox h))
    $ \i -> Comp <$> M.basicUnsafeGrow (hlookupC i v) n

instance WrapForall U.Unbox h (x ': xs) => U.Unbox (h :* (x ': xs))

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

instance WrapForall Hashable h xs => Hashable (h :| xs) where
  hashWithSalt s (EmbedAt i h) = views (pieceAt i)
    (\(Comp Dict) -> s `hashWithSalt` i `hashWithSalt` h)
    (library :: Comp Dict (Instance1 Hashable h) :* xs)
  {-# INLINE hashWithSalt #-}

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

#if !MIN_VERSION_vector(0,12,1)
newtype instance U.MVector s (Identity a) = MV_Identity (U.MVector s a)
newtype instance U.Vector (Identity a) = V_Identity (U.Vector a)

instance (U.Unbox a) => M.MVector U.MVector (Identity a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Identity v) = M.basicLength v
  basicUnsafeSlice i n (MV_Identity v) = MV_Identity $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Identity v1) (MV_Identity v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Identity <$> M.basicUnsafeNew n
  basicInitialize (MV_Identity v) = M.basicInitialize v
  basicUnsafeReplicate n (Identity x) = MV_Identity <$> M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_Identity v) i = Identity <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Identity v) i (Identity x) = M.basicUnsafeWrite v i x
  basicClear (MV_Identity v) = M.basicClear v
  basicSet (MV_Identity v) (Identity x) = M.basicSet v x
  basicUnsafeCopy (MV_Identity v1) (MV_Identity v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Identity v1) (MV_Identity v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Identity v) n = MV_Identity <$> M.basicUnsafeGrow v n

instance (U.Unbox a) => G.Vector U.Vector (Identity a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (MV_Identity v) = V_Identity <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Identity v) = MV_Identity <$> G.basicUnsafeThaw v
  basicLength (V_Identity v) = G.basicLength v
  basicUnsafeSlice i n (V_Identity v) = V_Identity $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Identity v) i = Identity <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Identity mv) (V_Identity v) = G.basicUnsafeCopy mv v

instance (U.Unbox a) => U.Unbox (Identity a)

newtype instance U.MVector s (Const' a b) = MV_Const (U.MVector s a)
newtype instance U.Vector (Const' a b) = V_Const (U.Vector a)

instance (U.Unbox a) => M.MVector U.MVector (Const' a b) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Const v) = M.basicLength v
  basicUnsafeSlice i n (MV_Const v) = MV_Const $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Const v1) (MV_Const v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Const <$> M.basicUnsafeNew n
  basicInitialize (MV_Const v) = M.basicInitialize v
  basicUnsafeReplicate n (Const' x) = MV_Const <$> M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_Const v) i = Const' <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Const v) i (Const' x) = M.basicUnsafeWrite v i x
  basicClear (MV_Const v) = M.basicClear v
  basicSet (MV_Const v) (Const' x) = M.basicSet v x
  basicUnsafeCopy (MV_Const v1) (MV_Const v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Const v1) (MV_Const v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Const v) n = MV_Const <$> M.basicUnsafeGrow v n

instance (U.Unbox a) => G.Vector U.Vector (Const' a b) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (MV_Const v) = V_Const <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Const v) = MV_Const <$> G.basicUnsafeThaw v
  basicLength (V_Const v) = G.basicLength v
  basicUnsafeSlice i n (V_Const v) = V_Const $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Const v) i = Const' <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Const mv) (V_Const v) = G.basicUnsafeCopy mv v

instance (U.Unbox a) => U.Unbox (Const' a b)
#endif
