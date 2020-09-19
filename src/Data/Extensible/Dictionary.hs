{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Dictionary
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Reification of constraints using extensible data types.
-- Also includes orphan instances.
-----------------------------------------------------------------------
module Data.Extensible.Dictionary (library, WrapForall, Instance1, And) where
import Control.DeepSeq
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
#ifdef CASSAVA
import qualified Data.Csv as Csv
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
#endif
import Data.Extensible.Class
import Data.Extensible.Field
import Data.Extensible.Product
import Data.Extensible.Sum
import Data.Extensible.Internal.Rig
import Data.Extensible.Nullable
import Data.Constraint
import Data.Extensible.Struct
import Data.Extensible.Wrapper
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Incremental
import Data.Maybe (isJust)
import Data.Monoid (Any(..))
import Data.Text.Prettyprint.Doc
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Text as T
import Data.Type.Equality
import qualified Language.Haskell.TH.Lift as TH
import Language.Haskell.TH hiding (Type)
import GHC.TypeLits
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Type.Membership

#ifdef BARBIES
import Barbies
import Data.Functor.Product
#endif

-- | Reify a collection of dictionaries, as you wish.
library :: forall c xs. Forall c xs => xs :& Comp Dict c
library = hrepeatFor (Proxy :: Proxy c) $ Comp Dict
{-# INLINE library #-}

class (f x, g x) => And f g x
instance (f x, g x) => And f g x

instance WrapForall Show h xs => Show (xs :& h) where
  showsPrec d xs = showParen (d > 0)
    $ henumerateFor (Proxy :: Proxy (Instance1 Show h)) (Proxy :: Proxy xs)
    (\i r -> showsPrec 0 (hlookup i xs) . showString " <: " . r)
    (showString "nil")

instance WrapForall Pretty h xs => Pretty (xs :& h) where
  pretty xs = align
    $ encloseSep (flatAlt "" "{ ") (flatAlt "" " }") (flatAlt "" "; ")
    $ henumerateFor (Proxy :: Proxy (Instance1 Pretty h)) (Proxy :: Proxy xs)
    (\i r -> pretty (hlookup i xs) : r)
    []


instance WrapForall Eq h xs => Eq (xs :& h) where
  xs == ys = henumerateFor (Proxy :: Proxy (Instance1 Eq h)) (Proxy :: Proxy xs)
    (\i r -> hlookup i xs == hlookup i ys && r) True
  {-# INLINE (==) #-}

instance (Eq (xs :& h), WrapForall Ord h xs) => Ord (xs :& h) where
  compare xs ys = henumerateFor (Proxy :: Proxy (Instance1 Ord h)) (Proxy :: Proxy xs)
    (\i r -> (hlookup i xs `compare` hlookup i ys) `mappend` r) mempty
  {-# INLINE compare #-}

instance WrapForall Semigroup h xs => Semigroup (xs :& h) where
  (<>) = hzipWith3 (\(Comp Dict) -> (<>))
    (library :: xs :& Comp Dict (Instance1 Semigroup h))
  {-# INLINE (<>) #-}

instance (WrapForall Semigroup h xs, WrapForall Monoid h xs) => Monoid (xs :& h) where
  mempty = hrepeatFor (Proxy :: Proxy (Instance1 Monoid h)) mempty
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

instance WrapForall Hashable h xs => Hashable (xs :& h) where
  hashWithSalt = hfoldlWithIndexFor (Proxy :: Proxy (Instance1 Hashable h))
    (const hashWithSalt)
  {-# INLINE hashWithSalt #-}

instance WrapForall Bounded h xs => Bounded (xs :& h) where
  minBound = hrepeatFor (Proxy :: Proxy (Instance1 Bounded h)) minBound
  maxBound = hrepeatFor (Proxy :: Proxy (Instance1 Bounded h)) maxBound

#if !MIN_VERSION_th_lift(0,7,9)
instance TH.Lift a => TH.Lift (Identity a) where
  lift = appE (conE 'Identity) . TH.lift . runIdentity

instance TH.Lift a => TH.Lift (Const a b) where
  lift = appE (conE 'Const) . TH.lift . getConst
#endif

instance WrapForall TH.Lift h xs => TH.Lift (xs :& h) where
  lift = hfoldrWithIndexFor (Proxy :: Proxy (Instance1 TH.Lift h))
    (\_ x xs -> infixE (Just $ TH.lift x) (varE '(<:)) (Just xs)) (varE 'nil)

newtype instance U.MVector s (xs :& h) = MV_Product (xs :& Comp (U.MVector s) h)
newtype instance U.Vector (xs :& h) = V_Product (xs :& Comp U.Vector h)

hlookupC :: Membership xs a -> xs :& Comp f g -> f (g a)
hlookupC i = getComp . hlookup i

instance WrapForall U.Unbox h (x ': xs) => G.Vector U.Vector ((x ': xs) :& h) where
  basicUnsafeFreeze (MV_Product v) = fmap V_Product
    $ hgenerateFor (Proxy :: Proxy (Instance1 U.Unbox h))
    $ \m -> Comp <$> G.basicUnsafeFreeze (hlookupC m v)
  basicUnsafeThaw (V_Product v) = fmap MV_Product
    $ hgenerateFor (Proxy :: Proxy (Instance1 U.Unbox h))
    $ \m -> Comp <$> G.basicUnsafeThaw (hlookupC m v)
  basicLength (V_Product v) = G.basicLength $ getComp $ hindex v leadership
  basicUnsafeSlice i n (V_Product v) = V_Product
    $ htabulateFor (Proxy :: Proxy (Instance1 U.Unbox h))
    $ \m -> Comp $ G.basicUnsafeSlice i n (hlookupC m v)
  basicUnsafeIndexM (V_Product v) i = hgenerateFor (Proxy :: Proxy (Instance1 U.Unbox h))
    $ \m -> G.basicUnsafeIndexM (hlookupC m v) i
  basicUnsafeCopy (MV_Product v) (V_Product w)
    = henumerateFor (Proxy :: Proxy (Instance1 U.Unbox h)) (Proxy :: Proxy (x ': xs)) ((>>) . \i -> G.basicUnsafeCopy (hlookupC i v) (hlookupC i w)) (return ())

instance WrapForall U.Unbox h (x ': xs) => M.MVector U.MVector ((x ': xs) :& h) where
  basicLength (MV_Product v) = M.basicLength $ getComp $ hindex v leadership
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
  basicInitialize (MV_Product v) = henumerateFor (Proxy :: Proxy (Instance1 U.Unbox h)) (Proxy :: Proxy (x ': xs)) ((>>) . \i -> M.basicInitialize $ hlookupC i v) (return ())
  basicUnsafeReplicate n x = fmap MV_Product
    $ hgenerateFor (Proxy :: Proxy (Instance1 U.Unbox h))
    $ \m -> fmap Comp $ M.basicUnsafeReplicate n $ hlookup m x
  basicUnsafeRead (MV_Product v) i = hgenerateFor (Proxy :: Proxy (Instance1 U.Unbox h))
    (\m -> M.basicUnsafeRead (hlookupC m v) i)
  basicUnsafeWrite (MV_Product v) i x = henumerateFor (Proxy :: Proxy (Instance1 U.Unbox h)) (Proxy :: Proxy (x ': xs)) ((>>) . \m -> M.basicUnsafeWrite (hlookupC m v) i (hlookup m x)) (return ())
  basicClear (MV_Product v) = henumerateFor (Proxy :: Proxy (Instance1 U.Unbox h)) (Proxy :: Proxy (x ': xs)) ((>>) . \i -> M.basicClear $ hlookupC i v) (return ())
  basicSet (MV_Product v) x = henumerateFor (Proxy :: Proxy (Instance1 U.Unbox h)) (Proxy :: Proxy (x ': xs)) ((>>) . \i -> M.basicSet (hlookupC i v) (hlookup i x)) (return ())
  basicUnsafeCopy (MV_Product v1) (MV_Product v2)
    = henumerateFor (Proxy :: Proxy (Instance1 U.Unbox h)) (Proxy :: Proxy (x ': xs)) ((>>) . \i -> M.basicUnsafeCopy (hlookupC i v1) (hlookupC i v2)) (return ())
  basicUnsafeMove (MV_Product v1) (MV_Product v2)
    = henumerateFor (Proxy :: Proxy (Instance1 U.Unbox h)) (Proxy :: Proxy (x ': xs)) ((>>) . \i -> M.basicUnsafeMove (hlookupC i v1) (hlookupC i v2)) (return ())
  basicUnsafeGrow (MV_Product v) n = fmap MV_Product
    $ hgenerateFor (Proxy :: Proxy (Instance1 U.Unbox h))
    $ \i -> Comp <$> M.basicUnsafeGrow (hlookupC i v) n

instance WrapForall U.Unbox h (x ': xs) => U.Unbox ((x ': xs) :& h)

instance WrapForall Arbitrary h xs => Arbitrary (xs :& h) where
  arbitrary = hgenerateFor (Proxy :: Proxy (Instance1 Arbitrary h)) (const arbitrary)
  shrink xs = henumerateFor (Proxy :: Proxy (Instance1 Arbitrary h))
    (Proxy :: Proxy xs) (\i -> (++)
    $ map (\x -> hmodify (\s -> set s i x) xs) $ shrink $ hindex xs i)
    []

instance WrapForall NFData h xs => NFData (xs :& h) where
  rnf xs = henumerateFor (Proxy :: Proxy (Instance1 NFData h)) (Proxy :: Proxy xs)
    (\i -> deepseq (hlookup i xs)) ()
  {-# INLINE rnf #-}

#ifdef CASSAVA
instance WrapForall Csv.FromField h xs => Csv.FromRecord (xs :& h) where
  parseRecord rec = hgenerateFor (Proxy :: Proxy (Instance1 Csv.FromField h))
    $ \i -> G.indexM rec (getMemberId i) >>= Csv.parseField

instance Forall (KeyTargetAre KnownSymbol (Instance1 Csv.FromField h)) xs => Csv.FromNamedRecord (xs :& Field h) where
  parseNamedRecord rec = hgenerateFor (Proxy :: Proxy (KeyTargetAre KnownSymbol (Instance1 Csv.FromField h)))
    $ \i -> rec Csv..: BC.pack (symbolVal (proxyKeyOf i)) >>= Csv.parseField

instance WrapForall Csv.ToField h xs => Csv.ToRecord (xs :& h) where
  toRecord = V.fromList
    . hfoldrWithIndexFor (Proxy :: Proxy (Instance1 Csv.ToField h))
      (\_ v -> (:) $ Csv.toField v) []

instance Forall (KeyTargetAre KnownSymbol (Instance1 Csv.ToField h)) xs => Csv.ToNamedRecord (xs :& Field h) where
  toNamedRecord = hfoldlWithIndexFor (Proxy :: Proxy (KeyTargetAre KnownSymbol (Instance1 Csv.ToField h)))
    (\k m v -> HM.insert (BC.pack (symbolVal (proxyKeyOf k))) (Csv.toField v) m)
    HM.empty

instance Forall (KeyIs KnownSymbol) xs => Csv.DefaultOrdered (RecordOf h xs) where
  headerOrder _ = V.fromList $ henumerateFor
    (Proxy :: Proxy (KeyIs KnownSymbol))
    (Proxy :: Proxy xs)
    (\k r -> stringKeyOf k : r) []
#endif

-- | @'parseJSON' 'J.Null'@ is called for missing fields.
instance Forall (KeyTargetAre KnownSymbol (Instance1 J.FromJSON h)) xs => J.FromJSON (xs :& Field h) where
  parseJSON = J.withObject "Object" $ \v -> hgenerateFor
    (Proxy :: Proxy (KeyTargetAre KnownSymbol (Instance1 J.FromJSON h)))
    $ \m -> let k = symbolVal (proxyKeyOf m)
      in fmap Field $ J.prependFailure ("parsing #" ++ k ++ ": ") $ J.parseJSON $ maybe J.Null id $ HM.lookup (T.pack k) v

instance Forall (KeyTargetAre KnownSymbol (Instance1 J.ToJSON h)) xs => J.ToJSON (xs :& Field h) where
  toJSON = J.Object . hfoldlWithIndexFor
    (Proxy :: Proxy (KeyTargetAre KnownSymbol (Instance1 J.ToJSON h)))
    (\k m v -> HM.insert (T.pack (symbolVal (proxyKeyOf k))) (J.toJSON v) m)
    HM.empty

instance Forall (KeyTargetAre KnownSymbol (Instance1 J.FromJSON h)) xs => J.FromJSON (xs :& Nullable (Field h)) where
  parseJSON = J.withObject "Object" $ \v -> hgenerateFor
    (Proxy :: Proxy (KeyTargetAre KnownSymbol (Instance1 J.FromJSON h)))
    $ \m -> let k = symbolVal (proxyKeyOf m)
      in fmap Nullable $ traverse J.parseJSON $ HM.lookup (T.pack k) v

instance Forall (KeyTargetAre KnownSymbol (Instance1 J.ToJSON h)) xs => J.ToJSON (xs :& Nullable (Field h)) where
  toJSON = J.Object . hfoldlWithIndexFor
    (Proxy :: Proxy (KeyTargetAre KnownSymbol (Instance1 J.ToJSON h)))
    (\k m (Nullable v) -> maybe id (HM.insert (T.pack $ symbolVal $ proxyKeyOf k) . J.toJSON) v m)
    HM.empty

instance WrapForall Show h xs => Show (xs :/ h) where
  showsPrec d (EmbedAt i h) = showParen (d > 10) $ showString "EmbedAt "
    . showsPrec 11 i
    . showString " "
    . views (pieceAt i) (\(Comp Dict) -> showsPrec 11 h) (library :: xs :& Comp Dict (Instance1 Show h))

instance WrapForall Eq h xs => Eq (xs :/ h) where
  EmbedAt p g == EmbedAt q h = case compareMembership p q of
    Left _ -> False
    Right Refl -> views (pieceAt p) (\(Comp Dict) -> g == h) (library :: xs :& Comp Dict (Instance1 Eq h))
  {-# INLINE (==) #-}

instance (Eq (xs :/ h), WrapForall Ord h xs) => Ord (xs :/ h) where
  EmbedAt p g `compare` EmbedAt q h = case compareMembership p q of
    Left x -> x
    Right Refl -> views (pieceAt p) (\(Comp Dict) -> compare g h) (library :: xs :& Comp Dict (Instance1 Ord h))
  {-# INLINE compare #-}

instance WrapForall NFData h xs => NFData (xs :/ h) where
  rnf (EmbedAt i h) = views (pieceAt i) (\(Comp Dict) -> rnf h) (library :: xs :& Comp Dict (Instance1 NFData h))
  {-# INLINE rnf #-}

instance WrapForall Hashable h xs => Hashable (xs :/ h) where
  hashWithSalt s (EmbedAt i h) = views (pieceAt i)
    (\(Comp Dict) -> s `hashWithSalt` i `hashWithSalt` h)
    (library :: xs :& Comp Dict (Instance1 Hashable h))
  {-# INLINE hashWithSalt #-}

instance WrapForall TH.Lift h xs => TH.Lift (xs :/ h) where
  lift (EmbedAt i h) = views (pieceAt i)
    (\(Comp Dict) -> conE 'EmbedAt `appE` TH.lift i `appE` TH.lift h)
    (library :: xs :& Comp Dict (Instance1 TH.Lift h))

instance WrapForall Arbitrary h xs => Arbitrary (xs :/ h) where
  arbitrary = choose (0, hcount (Proxy :: Proxy xs)) >>= henumerateFor
      (Proxy :: Proxy (Instance1 Arbitrary h))
      (Proxy :: Proxy xs)
      (\m r i -> if i == 0
        then EmbedAt m <$> arbitrary
        else r (i - 1))
        (error "Impossible")
  shrink (EmbedAt i h) = views (pieceAt i)
    (\(Comp Dict) -> EmbedAt i <$> shrink h)
    (library :: xs :& Comp Dict (Instance1 Arbitrary h))

instance WrapForall Pretty h xs => Pretty (xs :/ h) where
  pretty (EmbedAt i h) = "EmbedAt "
    <> pretty i
    <> " "
    <> views (pieceAt i) (\(Comp Dict) -> pretty h)
    (library :: xs :& Comp Dict (Instance1 Pretty h))

-- | Forall upon a wrapper
type WrapForall c h = Forall (Instance1 c h)

-- | Composition for a class and a wrapper
class c (h x) => Instance1 c h x
instance c (h x) => Instance1 c h x

#ifdef BARBIES
instance FunctorB ((:&) xs) where
  bmap = hmap

instance FunctorB ((:/) xs) where
  bmap = hoist

instance TraversableB ((:&) xs) where
  btraverse = htraverse

instance TraversableB ((:/) xs) where
  btraverse f (EmbedAt i x) = EmbedAt i <$> f x

instance Generate xs => ApplicativeB ((:&) xs) where
  bprod = hzipWith Pair
  bpure = hrepeat

instance ConstraintsB ((:&) xs) where
  type AllB c ((:&) xs) = Forall c xs
  baddDicts = bprod bdicts

instance ConstraintsB ((:/) xs) where
  type AllB c ((:/) xs) = Forall c xs
  baddDicts (EmbedAt i x) = EmbedAt i (Pair (hlookup i bdicts) x)

#endif

instance WrapForall Incremental h xs => Incremental (xs :& h) where
  type Delta (xs :& h) = xs :& WrapDelta h
  patch r =
    hmapWithIndexFor
      (Proxy :: Proxy (Instance1 Incremental h))
      (\i (WrapDelta d) -> maybe (hlookup i r) (patch (hlookup i r)) d)
  diff r =
    check
      . hmapWithIndexFor
        (Proxy :: Proxy (Instance1 Incremental h))
        (\i x -> WrapDelta (diff (hlookup i r) x))
    where
      check t
        | getAny $ hfoldMap (Any . isJust . unwrapDelta) t = Just t
        | otherwise = Nothing
