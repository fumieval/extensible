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

-- | Restricted version of '_Wrapper'.
-- It is useful for eliminating ambiguousness.
_WrapperAs :: (Functor f, Profunctor p, Wrapper h) => proxy v -> p (Repr h v) (f (Repr h v)) -> p (h v) (f (h v))
_WrapperAs _ = _Wrapper
{-# INLINE _WrapperAs #-}

-- | The extensible data types should take @k -> *@ as a parameter.
-- This class allows us to take a shortcut for direct representation.
class Wrapper (h :: k -> *) where
  -- | @'Repr' h v@ is the actual representation of @h v@.
  type Repr h (v :: k) :: *

  -- | This is an isomorphism between @h v@ and @'Repr' h v@.
  --
  -- @_Wrapper :: Iso' (h v) (Repr h v)@
  --
  _Wrapper :: (Functor f, Profunctor p) => Optic' p f (h v) (Repr h v)

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

-- | Turn a wrapper type into a clause for it.
newtype Match h r x = Match { runMatch :: h x -> r } deriving Typeable

instance Wrapper h => Wrapper (Match h r) where
  type Repr (Match h r) x = Repr h x -> r
  _Wrapper = withIso _Wrapper $ \f g -> dimap ((. g) .# runMatch) (fmap (Match #. (. f)))
  {-# INLINE _Wrapper #-}

-- | Poly-kinded composition
newtype Comp (f :: j -> *) (g :: i -> j) (a :: i) = Comp { getComp :: f (g a) }

comp :: Functor f => (a -> g b) -> f a -> Comp f g b
comp f = Comp #. fmap f
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
