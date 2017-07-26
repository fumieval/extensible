{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Wrapper
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-----------------------------------------------------------------------------
module Data.Extensible.Wrapper (
  Wrapper(..)
  , _WrapperAs
  , Const'(..)
  , Comp(..)
  , comp
  , Prod(..)
  ) where

import Control.DeepSeq
import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))
import Data.Profunctor.Unsafe (Profunctor(..))
import Data.Functor.Identity (Identity(..))
import Data.Extensible.Internal.Rig
import GHC.Generics (Generic)

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

-- | Restricted version of '_Wrapper'.
-- It is useful for eliminating ambiguousness.
_WrapperAs :: (Functor f, Profunctor p, Wrapper h) => proxy v -> Optic' p f (h v) (Repr h v)
_WrapperAs _ = _Wrapper
{-# INLINE _WrapperAs #-}

instance Wrapper Identity where
  type Repr Identity a = a
  _Wrapper = dimap runIdentity (fmap Identity)
  {-# INLINE _Wrapper #-}

instance Wrapper Maybe where
  type Repr Maybe a = Maybe a
  _Wrapper = id

instance Wrapper [] where
  type Repr [] a = [a]
  _Wrapper = id

-- | Poly-kinded composition
newtype Comp (f :: j -> *) (g :: i -> j) (a :: i) = Comp { getComp :: f (g a) }
  deriving (Show, Eq, Ord, Typeable, NFData, Generic)

-- | Wrap a result of 'fmap'
comp :: Functor f => (a -> g b) -> f a -> Comp f g b
comp f = Comp #. fmap f
{-# INLINE comp #-}

instance (Functor f, Wrapper g) => Wrapper (Comp f g) where
  type Repr (Comp f g) x = f (Repr g x)
  _Wrapper = withIso _Wrapper $ \f g -> dimap (fmap f .# getComp) (fmap (comp g))
  {-# INLINE _Wrapper #-}

-- | Poly-kinded Const
newtype Const' a x = Const' { getConst' :: a }
  deriving (Show, Eq, Ord, Typeable, Generic, NFData)

instance Wrapper (Const' a) where
  type Repr (Const' a) b = a
  _Wrapper = dimap getConst' (fmap Const')
  {-# INLINE _Wrapper #-}

instance Wrapper Proxy where
  type Repr Proxy x = ()
  _Wrapper = dimap (const ()) (fmap (const Proxy))
  {-# INLINE _Wrapper #-}

-- | Poly-kinded product
data Prod f g a = Prod (f a) (g a)
  deriving (Show, Eq, Ord, Typeable, Generic)

instance (NFData (f a), NFData (g a)) => NFData (Prod f g a)

instance (Wrapper f, Wrapper g) => Wrapper (Prod f g) where
  type Repr (Prod f g) a = (Repr f a, Repr g a)
  _Wrapper = dimap (\(Prod f g) -> (view _Wrapper f, view _Wrapper g))
    $ fmap (\(a, b) -> review _Wrapper a `Prod` review _Wrapper b)
