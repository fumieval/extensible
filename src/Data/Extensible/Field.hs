{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses, TypeInType #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Field
-- Copyright   :  (c) Fumiaki Kinoshita 2017
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
  , AssocKey
  , AssocValue
  , KeyValue
  , proxyAssocKey
  , proxyAssocValue
  , KeyIs
  , ValueIs
  -- * Internal
  , LabelPhantom
  , Labelling
  , Inextensible
  ) where
import Control.DeepSeq (NFData)
import Data.Coerce
#if __GLASGOW_HASKELL__ < 802
import Data.Constraint
#endif
import qualified Data.Csv as Csv
import Data.Extensible.Class
import Data.Extensible.Sum
import Data.Extensible.Match
import Data.Extensible.Product
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
#if __GLASGOW_HASKELL__ >= 800
import Data.Kind
#endif
import Data.Profunctor.Unsafe
import Data.Extensible.Wrapper
import Data.Functor.Identity
import Data.Hashable
import Data.Semigroup
import Data.Typeable (Typeable)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GHC.TypeLits hiding (Nat)
import Test.QuickCheck.Arbitrary

-- | Take the type of the key
type family AssocKey (kv :: Assoc k v) :: k where
  AssocKey (k ':> v) = k

-- | Proxy-level 'AssocKey'. This is useful when using 'symbolVal'.
proxyAssocKey :: proxy kv -> Proxy (AssocKey kv)
proxyAssocKey _ = Proxy

-- | Proxy-level 'AssocKey'. This is useful when using 'symbolVal'.
proxyAssocValue :: proxy kv -> Proxy (AssocValue kv)
proxyAssocValue _ = Proxy

-- | Take the type of the value
type family AssocValue (kv :: Assoc k v) :: v where
  AssocValue (k ':> v) = v

-- | Combined constraint for 'Assoc'
class (pk (AssocKey kv), pv (AssocValue kv)) => KeyValue pk pv kv where

instance (pk k, pv v) => KeyValue pk pv (k ':> v)

-- | Combined constraint for 'Assoc'
class (pk (AssocKey kv)) => KeyIs pk kv where

instance (pk k) => KeyIs pk (k ':> v)

-- | Combined constraint for 'Assoc'
class (pv (AssocValue kv)) => ValueIs pv kv where

instance (pv v) => ValueIs pv (k ':> v)


-- | A @'Field' h (k ':> v)@ is @h v@ annotated with the field name @k@.
--
-- @'Field' :: (v -> *) -> Assoc k v -> *@
--
#if __GLASGOW_HASKELL__ >= 800
newtype Field (h :: v -> Type) (kv :: Assoc k v)
#else
newtype Field (h :: v -> *) (kv :: Assoc k v)
#endif
  = Field { getField :: h (AssocValue kv) }

  deriving (Typeable, Generic)

#define ND_Field(c) deriving instance c (h (AssocValue kv)) => c (Field h kv)

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
ND_Field(Csv.FromField)

newtype instance U.MVector s (Field h x) = MV_Field (U.MVector s (h (AssocValue x)))
newtype instance U.Vector (Field h x) = V_Field (U.Vector (h (AssocValue x)))

instance (U.Unbox (h (AssocValue x))) => M.MVector U.MVector (Field h x) where
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

instance (U.Unbox (h (AssocValue x))) => G.Vector U.Vector (Field h x) where
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

instance (U.Unbox (h (AssocValue x))) => U.Unbox (Field h x)

-- | Lift a function for the content.
liftField :: (g (AssocValue kv) -> h (AssocValue kv)) -> Field g kv -> Field h kv
liftField = coerce
{-# INLINE liftField #-}

-- | Lift a function for the content.
liftField2 :: (f (AssocValue kv) -> g (AssocValue kv) -> h (AssocValue kv))
    -> Field f kv -> Field g kv -> Field h kv
liftField2 = coerce
{-# INLINE liftField2 #-}

instance Wrapper h => Wrapper (Field h) where
  type Repr (Field h) kv = Repr h (AssocValue kv)
  _Wrapper = dimap getField (fmap Field) . _Wrapper
  {-# INLINE _Wrapper #-}

-- | Shows in @field \@= value@ style instead of the derived one.
instance (KnownSymbol k, Wrapper h, Show (Repr h v)) => Show (Field h (k ':> v)) where
  showsPrec d (Field a) = showParen (d >= 1) $ showString (symbolVal (Proxy :: Proxy k))
    . showString " @= "
    . showsPrec 1 (view _Wrapper a)

-- | The type of records which contain several fields.
--
-- @RecordOf :: (v -> *) -> [Assoc k v] -> *@
--
type RecordOf h = (:*) (Field h)

-- | The dual of 'RecordOf'
--
-- @VariantOf :: (v -> *) -> [Assoc k v] -> *@
--
type VariantOf h = (:|) (Field h)

-- | Simple record
type Record = RecordOf Identity

-- | Simple variant
type Variant = VariantOf Identity

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
-- 'FieldOptic' "foo" = Associate "foo" a xs => Lens' ('Record' xs) a
-- 'FieldOptic' "foo" = Associate "foo" a xs => Prism' ('Variant' xs) a
-- @
--
-- 'FieldOptic's can be generated using 'mkField' defined in the "Data.Extensible.TH" module.
--
#if __GLASGOW_HASKELL__ >= 800
type FieldOptic k = forall kind. forall f p t xs (h :: kind -> Type) (v :: kind).
#else
type FieldOptic k = forall f p t xs (h :: kind -> *) (v :: kind).
#endif
  (Extensible f p t
  , ExtensibleConstr t (Field h) xs (k ':> v)
  , Associate k v xs
  , Labelling k p
  , Wrapper h)
  => Optic' p f (t (Field h) xs) (Repr h v)

#if __GLASGOW_HASKELL__ >= 800
-- | The trivial inextensible data type
data Inextensible (h :: k -> Type) (xs :: [k])
#else
data Inextensible (h :: k -> *) (xs :: [k])
#endif

instance (Functor f, Profunctor p) => Extensible f p Inextensible where
  pieceAt _ _ = error "Impossible"

-- | When you see this type as an argument, it expects a 'FieldLens'.
-- This type is used to resolve the name of the field internally.
type FieldName k = Optic' (LabelPhantom k) Proxy (Inextensible (Field Proxy) '[k ':> ()]) ()

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
-- foo = #num @= 42
--   <: #str @= "foo"
--   <: nil
-- @
(@=) :: Wrapper h => FieldName k -> Repr h v -> Field h (k ':> v)
(@=) _ = Field #. review _Wrapper
{-# INLINE (@=) #-}
infix 1 @=

-- | Lifted ('@=')
-- @
-- foo :: IO ('Record' '["num" >: Int, "str" >: String])
-- foo = hsequence
--   $ #num <@=> readLn
--   <: #str <@=> getLine
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

-- | Kind-monomorphic, unwrapped version of @('@=')@
(@==) :: FieldName (k :: Symbol) -> v -> Field Identity (k ':> v)
(@==) = (@=)
{-# INLINE (@==) #-}
infix 1 @==
