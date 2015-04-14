{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
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

_WrapperOn :: (Functor f, Profunctor p, Wrapper h v a) => (a -> h v) -> p a (f a) -> p (h v) (f (h v))
_WrapperOn _ = _Wrapper

class Wrapper (h :: k -> *) (v :: k) (a :: *) | h v -> a where
  _Wrapper :: (Functor f, Profunctor p) => p a (f a) -> p (h v) (f (h v))

instance Wrapper Identity a a where
  _Wrapper = dimap runIdentity (fmap Identity)

--------------------------------------------------------------------------

-- | Wrap a type that has a kind @* -> *@.
newtype K1 a f = K1 { getK1 :: f a } deriving (Eq, Ord, Read, Typeable)

instance Wrapper (K1 a) f (f a) where
  _Wrapper = dimap getK1 (fmap K1)

-- | The kind of key-value pairs
data Assoc k v = k :> v
infix 0 :>

data Field h kv where
  Field :: h v -> Field h (k ':> v)

instance Wrapper h v a => Wrapper (Field h) (k ':> v) a where
  _Wrapper = dimap (\(Field v) -> v) (fmap Field) . _Wrapper

-- | Shows in @field \@= value@ style instead of the derived one.
instance (KnownSymbol k, Wrapper h v a, Show a) => Show (Field h (k ':> v)) where
  showsPrec d (Field a) = showParen (d >= 1) $ showString (symbolVal (Proxy :: Proxy k))
    . showString " @= "
    . showsPrec 1 (view _Wrapper a)

-- | Turn a wrapper type into one clause that returns @a@.
newtype Match h r x = Match { runMatch :: h x -> r } deriving Typeable

instance Wrapper h v a => Wrapper (Match h r) v (a -> r) where
  _Wrapper = withIso _Wrapper $ \f g -> dimap ((. g) . runMatch) (fmap (Match . (. f)))

newtype Comp f g a = Comp { getComp :: f (g a) }

comp :: Functor f => (a -> g b) -> f a -> Comp f g b
comp f = Comp . fmap f
{-# INLINE comp #-}

instance (Functor f, Wrapper g v a) => Wrapper (Comp f g) v (f a) where
  _Wrapper = withIso _Wrapper $ \f g -> dimap (fmap f . getComp) (fmap (Comp . fmap g))

-- | Poly-kinded Const
newtype Const' a x = Const' { getConst' :: a } deriving Show

instance Wrapper (Const' a) x a where
  _Wrapper = dimap getConst' (fmap Const')

instance Wrapper Proxy x () where
  _Wrapper = dimap (const ()) (fmap (const Proxy))
