-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Rig
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Miscellaneous utilities
------------------------------------------------------------------------
module Data.Extensible.Internal.Rig where
import Unsafe.Coerce
import Control.Applicative
import Data.Profunctor
import Data.Functor.Identity
import Data.Tagged

-- | A type synonym for lenses
-- type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- | @'view' :: Lens' s a -> (a -> a) -> (s -> s)@
view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view l = views l id
{-# INLINE view #-}

-- | @'views' :: Lens' s a -> (a -> r) -> (s -> r)@
views :: ((a -> Const r a) -> s -> Const r s) -> (a -> r) -> s -> r
views = unsafeCoerce
{-# INLINE views #-}

-- | @'over' :: Lens' s a -> (a -> a) -> (s -> s)@
over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over = unsafeCoerce
{-# INLINE over #-}

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
  {-# INLINE dimap #-}

withIso :: (Exchange a b a (Identity b) -> Exchange a b s (Identity t)) -> ((s -> a) -> (b -> t) -> r) -> r
withIso l r = case l (Exchange id Identity) of
  Exchange f g -> r f (runIdentity . g)
{-# INLINE withIso #-}

review :: (Tagged a (Identity a) -> Tagged s (Identity s)) -> a -> s
review = unsafeCoerce
{-# INLINE review #-}
