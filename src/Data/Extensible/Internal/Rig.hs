{-# LANGUAGE PolyKinds, MultiParamTypeClasses, ConstraintKinds, UndecidableInstances, FlexibleInstances, DeriveFunctor #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable#-}
module Data.Extensible.Internal.Rig where
import Unsafe.Coerce
import Control.Applicative
import Data.Typeable
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

view :: ((a -> Const a a) -> (s -> Const a s)) -> s -> a
view l = views l id
{-# INLINE view #-}

views :: ((a -> Const r a) -> (s -> Const r s)) -> (a -> r) -> s -> r
views = unsafeCoerce
{-# INLINE views #-}

-- | Just a value.
newtype K0 a = K0 { getK0 :: a } deriving (Eq, Ord, Read, Typeable, Functor, Foldable, Traversable)

instance Applicative K0 where
  pure = K0
  K0 f <*> K0 a = K0 (f a)

instance Monad K0 where
  return = K0
  K0 a >>= k = k a

instance Show a => Show (K0 a) where
  showsPrec d (K0 a) = showParen (d > 10) $ showString "K0 " . showsPrec 11 a

over :: ((a -> K0 a) -> s -> K0 s) -> (a -> a) -> s -> s
over = unsafeCoerce
{-# INLINE over #-}

-- | Poly-kinded Const
newtype Const' a x = Const' { getConst' :: a } deriving Show

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

-- | Composition for a class and a wrapper
class c (h x) => ClassComp c h x
instance c (h x) => ClassComp c h x
