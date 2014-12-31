{-# LANGUAGE DataKinds, TypeOperators, PolyKinds, KindSignatures, GADTs, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Extensible ((∈)(), (:*)(Nil), (<:*), outI, (:|), (<:|), inU, K1(..), K2(..)) where
import Unsafe.Coerce
import Debug.Trace

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
  go _ Nil = error "Impossible"

inU :: (x ∈ xs) => h x -> h :| xs
inU = Union position

(<:|) :: (h x -> r) -> (h :* xs -> r) -> h :| (x ': xs) -> r
(<:|) r _ (Union (Position 0) h) = r (unsafeCoerce h)
(<:|) _ c (Union (Position n) h) = c $ unsafeCoerce $ Union (Position (n - 1)) h

data (h :: k -> *) :| (s :: [k]) where
  Union :: Position x xs -> h x -> h :| xs

newtype K1 a = K1 a deriving Show

newtype K2 a f = K2 (f a)

newtype Position x xs = Position { getPosition :: Int } deriving Show

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

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  (x ': xs) ++ ys = x ': (xs ++ ys)
  '[] ++ ys = ys

data Nat = Zero | DNat Nat | SDNat Nat | NotFound

class Record n where
  theInt :: Proxy n -> Int

instance Record Zero where
  theInt _ = 0

instance Record n => Record (DNat n) where
  theInt _ = theInt (Proxy :: Proxy n) * 2

instance Record n => Record (SDNat n) where
  theInt _ = theInt (Proxy :: Proxy n) * 2 + 1

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

data A = A Int deriving Show
data B = B Int deriving Show
data C = C Int deriving Show
data D = D Int deriving Show
data E = E Int deriving Show
data F = F Int deriving Show
data G = G Int deriving Show
data H = H Int deriving Show
data I = I Int deriving Show
data J = J Int deriving Show
data K = K Int deriving Show
data L = L Int deriving Show
data M = M Int deriving Show
data N = N Int deriving Show
data O = O Int deriving Show
data P = P Int deriving Show
data Q = Q Int deriving Show
data R = R Int deriving Show
data S = S Int deriving Show
data T = T Int deriving Show
data U = U Int deriving Show
data V = V Int deriving Show
data W = W Int deriving Show
data X = X Int deriving Show
data Y = Y Int deriving Show
data Z = Z Int deriving Show

blah26 :: K1 :* [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z]
blah26 = K1 (A 0)
    <:* K1 (B 0)
    <:* K1 (C 0)
    <:* K1 (D 0)
    <:* K1 (E 0)
    <:* K1 (F 0)
    <:* K1 (G 0)
    <:* K1 (H 0)
    <:* K1 (I 0)
    <:* K1 (J 0)
    <:* K1 (K 0)
    <:* K1 (L 0)
    <:* K1 (M 0)
    <:* K1 (N 0)
    <:* K1 (O 0)
    <:* K1 (P 0)
    <:* K1 (Q 0)
    <:* K1 (R 0)
    <:* K1 (S 0)
    <:* K1 (T 0)
    <:* K1 (U 0)
    <:* K1 (V 0)
    <:* K1 (W 0)
    <:* K1 (X 0)
    <:* K1 (Y 0)
    <:* K1 (Z 0)
    <:* Nil
