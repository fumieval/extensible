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
import Data.Coerce

type Optic' p f s a = p a (f a) -> p s (f s)

-- | @'view' :: Lens' s a -> (a -> a) -> (s -> s)@
view :: Optic' (->) (Const a) s a -> s -> a
view l = views l id
{-# INLINE view #-}

-- | @'views' :: Lens' s a -> (a -> r) -> (s -> r)@
views :: Optic' (->) (Const r) s a -> (a -> r) -> s -> r
views = unsafeCoerce
{-# INLINE views #-}

-- | @'over' :: Lens' s a -> (a -> a) -> (s -> s)@
over :: Optic' (->) Identity s a -> (a -> a) -> s -> s
over = unsafeCoerce
{-# INLINE over #-}

(.#) :: Coercible a b => (b -> c) -> (a -> b) -> (a -> c)
f .# _ = coerce f
{-# INLINE (.#) #-}

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
_ #. g = coerce g
{-# INLINE (#.) #-}

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
  {-# INLINE dimap #-}

withIso :: Optic' (Exchange a a) Identity s a -> ((s -> a) -> (a -> s) -> r) -> r
withIso l r = case l (Exchange id Identity) of
  Exchange f g -> r f (unsafeCoerce g)
{-# INLINE withIso #-}

review :: Optic' Tagged Identity s a -> a -> s
review = unsafeCoerce
{-# INLINE review #-}
