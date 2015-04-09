{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
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
import Data.Typeable
import Control.Applicative
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif
import Data.Profunctor

-- | A type synonym for lenses
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- | @'view' :: Lens' s a -> (a -> a) -> (s -> s)@
view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view l = views l id
{-# INLINE view #-}

-- | @'views' :: Lens' s a -> (a -> r) -> (s -> r)@
views :: ((a -> Const r a) -> s -> Const r s) -> (a -> r) -> s -> r
views = unsafeCoerce
{-# INLINE views #-}

-- | Just a value.
newtype K0 a = K0 { getK0 :: a } deriving (Eq, Ord, Read, Typeable, Functor, Foldable, Traversable)

_K0 :: (Functor f, Profunctor p) => p a (f b) -> p (K0 a) (f (K0 b))
_K0 = dimap getK0 (fmap K0)

instance Applicative K0 where
  pure = K0
  K0 f <*> K0 a = K0 (f a)

instance Monad K0 where
  return = K0
  K0 a >>= k = k a

instance Show a => Show (K0 a) where
  showsPrec d (K0 a) = showParen (d > 10) $ showString "K0 " . showsPrec 11 a

-- | @'over' :: Lens' s a -> (a -> a) -> (s -> s)@
over :: ((a -> K0 a) -> s -> K0 s) -> (a -> a) -> s -> s
over = unsafeCoerce
{-# INLINE over #-}

-- | Poly-kinded Const
newtype Const' a x = Const' { getConst' :: a } deriving Show

newtype Comp f g a = Comp { getComp :: f (g a) }

comp :: Functor f => (a -> g b) -> f a -> Comp f g b
comp f = Comp . fmap f
{-# INLINE comp #-}

-- | Poly-kinded Maybe
data Nullable h x = Null | Eine (h x) deriving (Show, Eq, Ord, Typeable)

-- | Destruct 'Nullable'.
nullable :: r -> (h x -> r) -> Nullable h x -> r
nullable r _ Null = r
nullable _ f (Eine h) = f h
{-# INLINE nullable #-}

-- | Apply a function to its content.
mapNullable :: (g x -> h y) -> Nullable g x -> Nullable h y
mapNullable f (Eine g) = Eine (f g)
mapNullable _ Null = Null
{-# INLINE mapNullable #-}

-- A list, but with Monoid instance based on merging
newtype MergeList a = MergeList { getMerged :: [a] } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Monoid (MergeList a) where
  mempty = MergeList []
  {-# INLINE mempty #-}
  mappend (MergeList a) (MergeList b) = MergeList $ merge a b where
    merge (x:xs) (y:ys) = x : y : merge xs ys
    merge xs [] = xs
    merge [] ys = ys
  {-# INLINE mappend #-}
