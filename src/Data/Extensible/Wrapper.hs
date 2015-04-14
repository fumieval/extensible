{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Extensible.Wrapper where

import Data.Typeable
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif
import Data.Profunctor
import Data.Functor.Identity
import Data.Extensible.Internal.Rig
import GHC.TypeLits hiding (Nat)

_WrapperAs :: (Functor f, Profunctor p, Wrapper h) => proxy v -> p (Repr h v) (f (Repr h v)) -> p (h v) (f (h v))
_WrapperAs _ = _Wrapper

class Wrapper (h :: k -> *) where
  type Repr h (v :: k) :: *
  _Wrapper :: (Functor f, Profunctor p) => p (Repr h v) (f (Repr h v)) -> p (h v) (f (h v))

instance Wrapper Identity where
  type Repr Identity a = a
  _Wrapper = dimap runIdentity (fmap Identity)
  {-# INLINE _Wrapper #-}

--------------------------------------------------------------------------

-- | Wrap a type that has a kind @* -> *@.
newtype K1 a f = K1 { getK1 :: f a } deriving (Eq, Ord, Read, Typeable)

instance Wrapper (K1 a) where
  type Repr (K1 a) f = f a
  _Wrapper = dimap getK1 (fmap K1)
  {-# INLINE _Wrapper #-}

-- | The kind of key-value pairs
data Assoc k v = k :> v
infix 0 :>

type family AssocValue (kv :: Assoc k v) :: v where
  AssocValue (k ':> v) = v

newtype Field (h :: v -> *) (kv :: Assoc k v) = Field (h (AssocValue kv))

instance Wrapper h => Wrapper (Field h) where
  type Repr (Field h) kv = Repr h (AssocValue kv)
  _Wrapper = dimap (\(Field v) -> v) (fmap Field) . _Wrapper
  {-# INLINE _Wrapper #-}

-- | Shows in @field \@= value@ style instead of the derived one.
instance (KnownSymbol k, Wrapper h, Show (Repr h v)) => Show (Field h (k ':> v)) where
  showsPrec d (Field a) = showParen (d >= 1) $ showString (symbolVal (Proxy :: Proxy k))
    . showString " @= "
    . showsPrec 1 (view _Wrapper a)

-- | Turn a wrapper type into one clause that returns @a@.
newtype Match h r x = Match { runMatch :: h x -> r } deriving Typeable

instance Wrapper h => Wrapper (Match h r) where
  type Repr (Match h r) x = Repr h x -> r
  _Wrapper = withIso _Wrapper $ \f g -> dimap ((. g) .# runMatch) (fmap (Match #. (. f)))
  {-# INLINABLE _Wrapper #-}

-- | Poly-kinded composition
newtype Comp (f :: j -> *) (g :: i -> j) (a :: i) = Comp { getComp :: f (g a) }

comp :: Functor f => (a -> g b) -> f a -> Comp f g b
comp f = Comp . fmap f
{-# INLINE comp #-}

instance (Functor f, Wrapper g) => Wrapper (Comp f g) where
  type Repr (Comp f g) x = f (Repr g x)
  _Wrapper = withIso _Wrapper $ \f g -> dimap (fmap f .# getComp) (fmap (Comp #. fmap g))
  {-# INLINE _Wrapper #-}

-- | Poly-kinded Const
newtype Const' a x = Const' { getConst' :: a } deriving Show

instance Wrapper (Const' a) where
  type Repr (Const' a) b = a
  _Wrapper = dimap getConst' (fmap Const')
  {-# INLINE _Wrapper #-}

instance Wrapper Proxy where
  type Repr Proxy x = ()
  _Wrapper = dimap (const ()) (fmap (const Proxy))
  {-# INLINE _Wrapper #-}
