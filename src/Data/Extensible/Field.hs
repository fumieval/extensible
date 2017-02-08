{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Record
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Flexible records and variants
-- Example: <https://github.com/fumieval/extensible/blob/master/examples/records.hs>
------------------------------------------------------------------------
module Data.Extensible.Field (
  Field(..)
  , (@=)
  , (<@=>)
  , (@:>)
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
  -- * Internal
  , LabelPhantom
  , Labelling
  , Inextensible
  ) where
import Control.DeepSeq (NFData)
import Data.Coerce
import Data.Extensible.Class
import Data.Extensible.Sum
import Data.Extensible.Match
import Data.Extensible.Product
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Profunctor.Unsafe
import Data.Constraint
import Data.Extensible.Wrapper
import Data.Functor.Identity
import Data.Semigroup
import Foreign.Storable (Storable)
import GHC.TypeLits hiding (Nat)

-- | Take the type of the key
type family AssocKey (kv :: Assoc k v) :: k where
  AssocKey (k ':> v) = k

-- | Proxy-level 'AssocKey'. This is useful when using 'symbolVal'.
proxyAssocKey :: proxy kv -> Proxy (AssocKey kv)
proxyAssocKey _ = Proxy

-- | Take the type of the value
type family AssocValue (kv :: Assoc k v) :: v where
  AssocValue (k ':> v) = v

class (pk (AssocKey kv), pv (AssocValue kv)) => KeyValue pk pv kv where

instance (pk k, pv v) => KeyValue pk pv (k ':> v)

-- | A @'Field' h (k ':> v)@ is @h v@ annotated with the field name @k@.
--
-- @'Field' :: (v -> *) -> Assoc k v -> *@
--
newtype Field (h :: v -> *) (kv :: Assoc k v) = Field { getField :: h (AssocValue kv) }

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

liftField :: (g (AssocValue kv) -> h (AssocValue kv)) -> Field g kv -> Field h kv
liftField = coerce
{-# INLINE liftField #-}

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
type FieldOptic k = forall kind. forall f p t xs (h :: kind -> *) (v :: kind).
#else
type FieldOptic k = forall f p t xs (h :: kind -> *) (v :: kind).
#endif
  (Extensible f p t
  , Associate k v xs
  , Labelling k p
  , Wrapper h)
  => Optic' p f (t (Field h) xs) (Repr h v)

-- | The trivial inextensible data type
data Inextensible (h :: k -> *) (xs :: [k])

instance (Functor f, Profunctor p) => Extensible f p Inextensible where
  pieceAt _ _ = error "Impossible"

-- | When you see this type as an argument, it expects a 'FieldLens'.
-- This type is used to resolve the name of the field internally.
type FieldName k = Optic' (LabelPhantom k) Proxy (Inextensible (Field Proxy) '[k ':> ()]) ()

type family Labelling s p :: Constraint where
  Labelling s (LabelPhantom t) = s ~ t
  Labelling s p = ()

-- | A ghostly type which spells the field name
data LabelPhantom s a b

instance Profunctor (LabelPhantom s) where
  dimap _ _ _ = error "Impossible"

-- | Annotate a value by the field name.
(@=) :: Wrapper h => FieldName k -> Repr h v -> Field h (k ':> v)
(@=) _ = Field #. review _Wrapper
{-# INLINE (@=) #-}
infix 1 @=

-- | Lifted ('@=')
(<@=>) :: (Functor f, Wrapper h) => FieldName k -> f (Repr h v) -> Comp f (Field h) (k ':> v)
(<@=>) k = Comp #. fmap (k @=)
{-# INLINE (<@=>) #-}
infix 1 <@=>

-- | Annotate a value by the field name without 'Wrapper'.
(@:>) :: FieldName k -> h v -> Field h (k ':> v)
(@:>) _ = Field
infix 1 @:>
