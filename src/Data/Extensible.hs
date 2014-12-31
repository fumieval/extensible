{-# LANGUAGE DataKinds, TypeOperators, PolyKinds, KindSignatures, GADTs, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Extensible (
  -- * Lookup
  Position
  , (∈)(..)
  -- * Product
  , (:*)(..)
  , (<:*)
  , outP
  -- * Sum
  , (:|)(..)
  , (<:|)
  , inS
  -- * Utilities
  , K1(..)
  , K2(..)
  , Match(..)
  , match
  ) where
import Unsafe.Coerce
import Data.Bits
import Data.Typeable

data (h :: k -> *) :* (s :: [k]) where
  Nil :: h :* '[]
  Tree :: h x
    -> h :* Half xs
    -> h :* Half (Tail xs)
    -> h :* (x ': xs)

instance Show (h :* '[]) where
  show Nil = "Nil"

instance (Show (h :* Half xs), Show (h :* Half (Tail xs)), Show (h x)) => Show (h :* (x ': xs)) where
  showsPrec d (Tree h a b) = showParen (d > 10) $ showString "Tree "
    . showsPrec 11 h
    . showChar ' '
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b

(<:*) :: h x -> h :* xs -> h :* (x ': xs)
a <:* Nil = Tree a Nil Nil
a <:* Tree b c d = Tree a (unsafeCoerce (<:*) b d) c --  (Half (x1 : xs1) ~ (x1 : Half (Tail xs1)))
infixr 6 <:*

outP :: forall h x xs. (x ∈ xs) => h :* xs -> h x
outP = productAt (position :: Position x xs) where

productAt :: forall h x xs. Position x xs -> h :* xs -> h x
productAt (Position x) = go x where
  go :: Int -> h :* xs -> h x
  go 0 (Tree h _ _) = unsafeCoerce h
  go n (Tree _ a b) = go (shiftR n 1) $ case n .&. 1 of
    0 -> unsafeCoerce a
    1 -> unsafeCoerce b
    _ -> error "GHC is bad at math"
  go _ Nil = error "Impossible"

inS :: (x ∈ xs) => h x -> h :| xs
inS = UnionAt position

(<:|) :: (h x -> r) -> (h :* xs -> r) -> h :| (x ': xs) -> r
(<:|) r _ (UnionAt (Position 0) h) = r (unsafeCoerce h)
(<:|) _ c (UnionAt (Position n) h) = c $ unsafeCoerce $ UnionAt (Position (n - 1)) h

data (h :: k -> *) :| (s :: [k]) where
  UnionAt :: Position x xs -> h x -> h :| xs

newtype K1 a = K1 a deriving (Show, Eq, Ord, Read, Typeable)

newtype K2 a f = K2 (f a) deriving (Show, Eq, Ord, Read, Typeable)

newtype Match h a x = Match { runMatch :: h x -> a }

match :: Match h a :* xs -> h :| xs -> a
match p (UnionAt pos h) = runMatch (productAt pos p) h

---------------------------------------------------------------------

newtype Position x xs = Position Int deriving Show

class (x :: k) ∈ (xs :: [k]) where
  position :: Position x xs

instance Record (Lookup x xs) => x ∈ xs where
  position = Position $ theInt (Proxy :: Proxy (Lookup x xs))
  {-# INLINE position #-}

type family Half (xs :: [k]) :: [k] where
  Half '[] = '[]
  Half (x ': y ': zs) = x ': zs
  Half (x ': '[]) = x ': '[]

type family Tail (xs :: [k]) :: [k] where
  Tail (x ': xs) = xs
  Tail '[] = '[]

data Nat = Zero | DNat Nat | SDNat Nat | NotFound

class Record n where
  theInt :: Proxy n -> Int

instance Record Zero where
  theInt _ = 0

instance Record n => Record (DNat n) where
  theInt _ = theInt (Proxy :: Proxy n) `shiftL` 1

instance Record n => Record (SDNat n) where
  theInt _ = (theInt (Proxy :: Proxy n) `shiftL` 1) .|. 1

instance Record n => Record (Just n) where
  theInt _ = theInt (Proxy :: Proxy n)

type family Lookup (x :: k) (xs :: [k]) :: Nat where
  Lookup x (x ': xs) = Zero
  Lookup x (y ': ys) = Succ (Lookup x ys)
  Lookup x '[] = NotFound

type family Succ (x :: Nat) :: Nat where
  Succ Zero = SDNat Zero
  Succ (DNat n) = SDNat n
  Succ (SDNat n) = DNat (Succ n)
  Succ NotFound = NotFound
