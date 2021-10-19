{-# LANGUAGE DeriveTraversable, StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Wrapper
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-----------------------------------------------------------------------------
module Data.Extensible.Wrapper (
  Wrapper(..)
  , _WrapperAs
  , type Comp
  , pattern Comp
  , getComp
  , comp
  , Prod(..)
  ) where

import Control.Applicative
import Control.DeepSeq
import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))
import Data.Profunctor.Unsafe (Profunctor(..))
import Data.Functor.Compose
import Data.Functor.Identity (Identity(..))
import Data.Extensible.Internal.Rig
import Data.Hashable
import Data.Kind (Type)
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary


-- | The extensible data types should take @k -> Type@ as a parameter.
-- This class allows us to take a shortcut for direct representation.
class Wrapper (h :: k -> Type) where
  -- | @'Repr' h v@ is the user-facing representation of @h v@.
  type Repr h (v :: k) :: Type

  -- | This is an isomorphism between @h v@ and @'Repr' h v@.
  --
  -- @_Wrapper :: Iso' (h v) (Repr h v)@
  --
  _Wrapper :: (Functor f, Profunctor p) => Optic' p f (h v) (Repr h v)
  _Wrapper = dimap unwrap (fmap wrap)
  {-# INLINE _Wrapper #-}

  wrap :: Repr h v -> h v
  wrap = review _Wrapper
  {-# INLINE wrap #-}

  unwrap :: h v -> Repr h v
  unwrap = view _Wrapper
  {-# INLINE unwrap #-}

  {-# MINIMAL wrap, unwrap | _Wrapper #-}

-- | Restricted version of '_Wrapper'.
-- It is useful for eliminating ambiguousness.
_WrapperAs :: (Functor f, Profunctor p, Wrapper h) => proxy v -> Optic' p f (h v) (Repr h v)
_WrapperAs _ = _Wrapper
{-# INLINE _WrapperAs #-}

instance Wrapper Identity where
  type Repr Identity a = a
  unwrap = runIdentity
  {-# INLINE unwrap #-}
  wrap = Identity
  {-# INLINE wrap #-}

instance Wrapper Maybe where
  type Repr Maybe a = Maybe a
  _Wrapper = id

instance Wrapper (Either e) where
  type Repr (Either e) a = Either e a
  _Wrapper = id

instance Wrapper [] where
  type Repr [] a = [a]
  _Wrapper = id

type Comp = Compose

pattern Comp :: f (g a) -> Compose f g a
pattern Comp a = Compose a
{-# DEPRECATED Comp "Use Compose instead" #-}


getComp :: Compose f g a -> f (g a)
getComp = getCompose
{-# DEPRECATED getComp "Use getCompose instead" #-}

-- | Wrap a result of 'fmap'
comp :: Functor f => (a -> g b) -> f a -> Compose f g b
comp f = Compose #. fmap f
{-# INLINE comp #-}

instance (Functor f, Wrapper g) => Wrapper (Compose f g) where
  type Repr (Compose f g) x = f (Repr g x)
  _Wrapper = withIso _Wrapper $ \f g -> dimap (fmap f .# getCompose) (fmap (comp g))
  {-# INLINE _Wrapper #-}

instance Wrapper (Const a) where
  type Repr (Const a) b = a
  wrap = Const
  {-# INLINE wrap #-}
  unwrap = getConst
  {-# INLINE unwrap #-}

instance Wrapper Proxy where
  type Repr Proxy x = ()
  wrap _ = Proxy
  {-# INLINE wrap #-}
  unwrap _ = ()
  {-# INLINE unwrap #-}

-- | Poly-kinded product
data Prod f g a = Prod (f a) (g a)
  deriving (Show, Eq, Ord, Typeable, Generic, Functor, Foldable, Traversable)

instance (NFData (f a), NFData (g a)) => NFData (Prod f g a)
instance (Hashable (f a), Hashable (g a)) => Hashable (Prod f g a)

instance (Wrapper f, Wrapper g) => Wrapper (Prod f g) where
  type Repr (Prod f g) a = (Repr f a, Repr g a)
  unwrap (Prod f g) = (unwrap f, unwrap g)
  {-# INLINE unwrap #-}
  wrap (f, g) = wrap f `Prod` wrap g
  {-# INLINE wrap #-}

instance (Semigroup (f a), Semigroup (g a)) => Semigroup (Prod f g a) where
  Prod a b <> Prod c d = Prod (a <> c) (b <> d)

instance (Monoid (f a), Monoid (g a)) => Monoid (Prod f g a) where
  mempty = Prod mempty mempty
  mappend = (<>)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Prod f g a) where
  arbitrary = Prod <$> arbitrary <*> arbitrary
  shrink (Prod a b) = Prod a `map` shrink b ++ flip Prod b `map` shrink a
