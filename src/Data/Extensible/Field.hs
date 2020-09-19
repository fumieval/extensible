{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableSuperClasses, TypeInType #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Field
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Flexible records and variants
------------------------------------------------------------------------
module Data.Extensible.Field (
  Field(..)
  , (@=)
  , (<@=>)
  , (@:>)
  , (@==)
  , FieldOptic
  , FieldName
  , liftField
  , liftField2
  -- * Records and variants
  , RecordOf
  , Record
  , emptyRecord
  , VariantOf
  , Variant
  -- * Matching
  , matchWithField
  , matchField
  -- * Key / value
  , KeyOf
  , proxyKeyOf
  , stringKeyOf
  , TargetOf
  , proxyTargetOf
  , KeyIs
  , TargetIs
  , KeyTargetAre
  -- * Internal
  , LabelPhantom
  , Labelling
  , Inextensible
  ) where
import Control.DeepSeq (NFData)
import qualified Data.Aeson as J
import Data.Coerce
#ifdef CASSAVA
import qualified Data.Csv as Csv
#endif
import Data.Extensible.Class
import Data.Extensible.Sum
import Data.Extensible.Match
import Data.Extensible.Product
import Data.Extensible.Internal.Rig
import Data.Kind
import Data.Profunctor.Unsafe
import Data.Extensible.Wrapper
import Data.Functor.Identity
import Data.Hashable
import Data.Incremental (Incremental)
import Data.String
import Data.Text.Prettyprint.Doc
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GHC.TypeLits hiding (Nat)
import Language.Haskell.TH.Lift
import Language.Haskell.TH (appE, conE)
import Test.QuickCheck.Arbitrary
import Type.Membership

-- | A @'Field' h (k ':> v)@ is @h v@ annotated with the field name @k@.
--
-- @'Field' :: (v -> Type) -> Assoc k v -> Type@
--
newtype Field (h :: v -> Type) (kv :: Assoc k v)
  = Field { getField :: h (TargetOf kv) }

  deriving (Generic)

#define ND_Field(c) deriving instance c (h (TargetOf kv)) => c (Field h kv)

ND_Field(Eq)
ND_Field(Ord)
ND_Field(Num)
ND_Field(Integral)
ND_Field(Fractional)
ND_Field(Floating)
ND_Field(Real)
ND_Field(RealFloat)
ND_Field(RealFrac)
ND_Field(Semigroup)
ND_Field(Storable)
ND_Field(Monoid)
ND_Field(Enum)
ND_Field(Bounded)
ND_Field(NFData)
ND_Field(Arbitrary)
ND_Field(Hashable)
ND_Field(J.FromJSON)
ND_Field(J.ToJSON)
#ifdef CASSAVA
ND_Field(Csv.FromField)
ND_Field(Csv.ToField)
#endif
ND_Field(Incremental)

newtype instance U.MVector s (Field h x) = MV_Field (U.MVector s (h (TargetOf x)))
newtype instance U.Vector (Field h x) = V_Field (U.Vector (h (TargetOf x)))

instance (U.Unbox (h (TargetOf x))) => M.MVector U.MVector (Field h x) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Field v) = M.basicLength v
  basicUnsafeSlice i n (MV_Field v) = MV_Field $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Field v1) (MV_Field v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Field <$> M.basicUnsafeNew n
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Field v) = M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif
  basicUnsafeReplicate n (Field x) = MV_Field <$> M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_Field v) i = Field <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Field v) i (Field x) = M.basicUnsafeWrite v i x
  basicClear (MV_Field v) = M.basicClear v
  basicSet (MV_Field v) (Field x) = M.basicSet v x
  basicUnsafeCopy (MV_Field v1) (MV_Field v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Field v1) (MV_Field v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Field v) n = MV_Field <$> M.basicUnsafeGrow v n

instance (U.Unbox (h (TargetOf x))) => G.Vector U.Vector (Field h x) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (MV_Field v) = V_Field <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Field v) = MV_Field <$> G.basicUnsafeThaw v
  basicLength (V_Field v) = G.basicLength v
  basicUnsafeSlice i n (V_Field v) = V_Field $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Field v) i = Field <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Field mv) (V_Field v) = G.basicUnsafeCopy mv v

instance (U.Unbox (h (TargetOf x))) => U.Unbox (Field h x)

instance Lift (h (TargetOf x)) => Lift (Field h x) where
  lift = appE (conE 'Field) . lift . getField

-- | Lift a function for the content.
liftField :: (g (TargetOf kv) -> h (TargetOf kv)) -> Field g kv -> Field h kv
liftField = coerce
{-# INLINE liftField #-}

-- | Lift a function for the content.
liftField2 :: (f (TargetOf kv) -> g (TargetOf kv) -> h (TargetOf kv))
    -> Field f kv -> Field g kv -> Field h kv
liftField2 = coerce
{-# INLINE liftField2 #-}

instance Wrapper h => Wrapper (Field h) where
  type Repr (Field h) kv = Repr h (TargetOf kv)
  _Wrapper = dimap getField (fmap Field) . _Wrapper
  {-# INLINE _Wrapper #-}

-- | Shows in @field \@= value@ style instead of the derived one.
instance (KnownSymbol k, Wrapper h, Show (Repr h v)) => Show (Field h (k ':> v)) where
  showsPrec d (Field a) = showParen (d >= 1) $ showString (symbolVal (Proxy :: Proxy k))
    . showString " @= "
    . showsPrec 1 (view _Wrapper a)

instance (KnownSymbol k, Pretty (h v)) => Pretty (Field h (k ':> v)) where
  pretty (Field a) = fromString (symbolVal (Proxy :: Proxy k))
    <> ": "
    <> pretty a

-- | The type of records which contain several fields.
--
-- @RecordOf :: (v -> Type) -> [Assoc k v] -> Type@
--
type RecordOf h xs = xs :& Field h

-- | The dual of 'RecordOf'
--
-- @VariantOf :: (v -> Type) -> [Assoc k v] -> Type@
--
type VariantOf h xs = xs :/ Field h

-- | Simple record
type Record xs = RecordOf Identity xs

-- | Simple variant
type Variant xs = VariantOf Identity xs

-- | An empty 'Record'.
emptyRecord :: Record '[]
emptyRecord = nil
{-# INLINE emptyRecord #-}

-- | Select a corresponding field of a variant.
matchWithField :: (forall x. f x -> g x -> r) -> RecordOf f xs -> VariantOf g xs -> r
matchWithField h = matchWith (\(Field x) (Field y) -> h x y)
{-# INLINE matchWithField #-}

-- | Pattern matching on a 'Variant'
matchField :: RecordOf (Match h r) xs -> VariantOf h xs -> r
matchField = matchWithField runMatch
{-# INLINE matchField #-}

-- | @FieldOptic s@ is a type of optics that points a field/constructor named @s@.
--
-- The yielding fields can be
-- <http://hackage.haskell.org/package/lens/docs/Control-Lens-Lens.html#t:Lens Lens>es
-- for 'Record's and
-- <http://hackage.haskell.org/package/lens/docs/Control-Lens-Lens.html#t:Prism Prism>s
-- for 'Variant's.
--
-- @
-- 'FieldOptic' "foo" = Lookup xs "foo" a => Lens' ('Record' xs) a
-- 'FieldOptic' "foo" = Lookup xs "foo" a => Prism' ('Variant' xs) a
-- @
--
-- 'FieldOptic's can be generated using 'mkField' defined in the "Data.Extensible.TH" module.
--
type FieldOptic k = forall kind. forall f p t xs (h :: kind -> Type) (v :: kind).
  (Extensible f p t
  , ExtensibleConstr t xs (Field h) (k ':> v)
  , Lookup xs k v
  , Labelling k p
  , Wrapper h)
  => Optic' p f (t xs (Field h)) (Repr h v)

-- | The trivial inextensible data type
data Inextensible (xs :: [k]) (h :: k -> Type)

instance (Functor f, Profunctor p) => Extensible f p Inextensible where
  pieceAt _ _ = error "Impossible"

-- | When you see this type as an argument, it expects a 'FieldLens'.
-- This type is used to resolve the name of the field internally.
type FieldName k = Optic' (LabelPhantom k) Proxy (Inextensible '[k ':> ()] (Field Proxy)) ()

-- | Signifies a field name internally
type family Labelling s p :: Constraint where
  Labelling s (LabelPhantom t) = s ~ t
  Labelling s p = ()

-- | A ghostly type which spells the field name
data LabelPhantom s a b

instance Profunctor (LabelPhantom s) where
  dimap _ _ _ = error "Impossible"

-- | Annotate a value by the field name.
--
-- @
-- foo :: 'Record' '["num" >: Int, "str" >: String]
-- foo = #num \@= 42
--   <: #str \@= "foo"
--   <: nil
-- @
(@=) :: Wrapper h => FieldName k -> Repr h v -> Field h (k ':> v)
(@=) _ = Field #. review _Wrapper
{-# INLINE (@=) #-}
infix 1 @=

-- | Lifted ('@=')
--
-- @
-- foo :: IO ('Record' '["num" >: Int, "str" >: String])
-- foo = hsequence
--   $ #num \<\@=\> readLn
--   <: #str \<\@=\> getLine
--   <: nil
-- @
(<@=>) :: (Functor f, Wrapper h) => FieldName k -> f (Repr h v) -> Comp f (Field h) (k ':> v)
(<@=>) k = comp (k @=)
{-# INLINE (<@=>) #-}
infix 1 <@=>

-- | Annotate a value by the field name without 'Wrapper'.
(@:>) :: FieldName k -> h v -> Field h (k ':> v)
(@:>) _ = Field
infix 1 @:>

-- | Kind-monomorphic, unwrapped version of '@='
(@==) :: FieldName (k :: Symbol) -> v -> Field Identity (k ':> v)
(@==) = (@=)
{-# INLINE (@==) #-}
infix 1 @==
