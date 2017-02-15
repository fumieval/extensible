{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Internal.Rig
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Re-implementation of lens combinators
------------------------------------------------------------------------
module Data.Extensible.Internal.Rig (
  Optic
  , Optic'
  , view
  , views
  , over
  , withIso
  , Exchange(..)
  , review
  )
where
import Control.Applicative
import Data.Profunctor
import Data.Functor.Identity
import Data.Tagged
import Data.Coerce

type Optic p f s t a b = p a (f b) -> p s (f t)
type Optic' p f s a = p a (f a) -> p s (f s)

-- | @'view' :: Getter s a -> s -> a@
view :: Optic' (->) (Const a) s a -> s -> a
view l = views l id
{-# INLINE view #-}

-- | @'views' :: Getter s a -> (a -> r) -> (s -> r)@
views :: Optic' (->) (Const r) s a -> (a -> r) -> s -> r
views = coerce
{-# INLINE views #-}

-- | @'over' :: Setter s t a b -> (a -> b) -> (s -> t)@
over :: Optic (->) Identity s t a b -> (a -> b) -> s -> t
over = coerce
{-# INLINE over #-}

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
  {-# INLINE dimap #-}

withIso :: Optic (Exchange a b) Identity s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso l r = case l (Exchange id Identity) of
  Exchange f g -> r f (coerce g)
{-# INLINE withIso #-}

review :: Optic' Tagged Identity s a -> a -> s
review = coerce
{-# INLINE review #-}
