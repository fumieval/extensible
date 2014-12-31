{-# LANGUAGE DataKinds, TypeOperators, PolyKinds, KindSignatures, GADTs, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Extensible ((∈)(), (:*), outI, (:|), inU, K1, K2) where
import Unsafe.Coerce

data Proxy a = Proxy

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

outI :: forall h x xs. (x ∈ xs) => h :* xs -> h x
outI = go $ getPosition (position :: Position x xs) where
  go :: Int -> h :* xs -> h x
  go 0 (Tree h _ _) = unsafeCoerce h
  go n (Tree h a b) = case divMod (n - 1) 2 of
    (m, 0) -> go m (unsafeCoerce a)
    (m, 1) -> go m (unsafeCoerce b)

inU :: (x ∈ xs) => h x -> h :| xs
inU = Union position

data (h :: k -> *) :| (s :: [k]) where
  Union :: Position x xs -> h x -> h :| xs

newtype K1 a = K1 a deriving Show

newtype K2 a f = K2 (f a)

newtype Position x xs = Position { getPosition :: Int } deriving Show

class (x :: k) ∈ (xs :: [k]) where
  position :: Position x xs

instance ToInt (Lookup x xs) => x ∈ xs where
  position = Position (theInt (Proxy :: Proxy (Lookup x xs)))

type family Half (xs :: [k]) :: [k] where
  Half '[] = '[]
  Half (x ': y ': zs) = x ': zs
  Half (x ': '[]) = x ': '[]

type family Tail (xs :: [k]) :: [k] where
  Tail (x ': xs) = xs
  Tail '[] = '[]

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  (x ': xs) ++ ys = x ': (xs ++ ys)
  '[] ++ ys = ys

data Nat = Zero | Succ Nat

class ToInt (n :: Nat) where
  theInt :: Proxy n -> Int

instance ToInt Zero where
  theInt _ = 0

instance ToInt n => ToInt (Succ n) where
  theInt _ = theInt (Proxy :: Proxy n) + 1

type family Lookup (x :: k) (xs :: [k]) :: Nat where
  Lookup x (x ': xs) = Zero
  Lookup x (y ': ys) = Succ (Lookup x ys)
