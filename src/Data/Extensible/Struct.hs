{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns, BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Data.Extensible.Struct (
  -- * Mutable struct
  Struct
  , set
  , get
  , new
  , newFor
  , newFromHList
  -- * Immutable product
  , (:*)
  , unsafeFreeze
  , newFrom
  , hlookup
  , hlength
  , hfoldrWithIndex
  , thaw
  , hfrozen
  , toHList) where

import GHC.Prim
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad
import Data.Extensible.Class
import Data.Extensible.Internal
import Control.Comonad
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import qualified Data.Extensible.HList as L
import GHC.Types

-- | Mutable type-indexed struct.
data Struct s (h :: k -> *) (xs :: [k]) = Struct (SmallMutableArray# s Any)

set :: PrimMonad m => Struct (PrimState m) h xs -> Membership xs x -> h x -> m ()
set (Struct m) (getMemberId -> I# i) e = primitive
  $ \s -> case unsafeCoerce# writeSmallArray# m i e s of
    s' -> (# s', () #)

get :: PrimMonad m => Struct (PrimState m) h xs -> Membership xs x -> m (h x)
get (Struct m) (getMemberId -> I# i) = primitive $ unsafeCoerce# readSmallArray# m i

-- | Create a new 'Struct' using the supplied initializer.
new :: forall h m xs. (PrimMonad m, Generate xs)
  => (forall x. Membership xs x -> h x)
  -> m (Struct (PrimState m) h xs)
new k = do
  let !(I# n) = henumerate (const (+1) :: Membership xs x -> Int -> Int) 0
  m <- primitive $ \s -> case newSmallArray# n undefined s of
    (# s', a #) -> (# s', Struct a #)
  henumerate (\i cont -> set m i (k i) >> cont) $ return m

-- | Create a new 'Struct' using the supplied initializer with a context.
newFor :: forall proxy c h m xs. (PrimMonad m, Forall c xs)
  => proxy c
  -> (forall x. c x => Membership xs x -> h x)
  -> m (Struct (PrimState m) h xs)
newFor p k = do
  let !(I# n) = henumerateFor p (Proxy :: Proxy xs) (const (+1) :: Membership xs x -> Int -> Int) 0
  m <- primitive $ \s -> case newSmallArray# n undefined s of
    (# s', a #) -> (# s', Struct a #)
  henumerateFor p (Proxy :: Proxy xs) (\i cont -> set m i (k i) >> cont) $ return m

newFromHList :: forall h m xs. PrimMonad m => L.HList h xs -> m (Struct (PrimState m) h xs)
newFromHList l = do
  let !(I# size) = L.length l
  m <- primitive $ \s -> case newSmallArray# size undefined s of
    (# s', a #) -> (# s', Struct a #)

  let go :: Int -> L.HList h t -> m ()
      go _ L.HNil = return ()
      go i (L.HCons x xs) = set m (unsafeMembership i) x >> go (i + 1) xs

  go 0 l
  return m

-- | The type of extensible products.
--
-- @(:*) :: (k -> *) -> [k] -> *@
--
data (h :: k -> *) :* (s :: [k]) = HProduct (SmallArray# Any)

-- | Turn 'Struct' into an immutable product. The original 'Struct' may not be used.
unsafeFreeze :: PrimMonad m => Struct (PrimState m) h xs -> m (h :* xs)
unsafeFreeze (Struct m) = primitive $ \s -> case unsafeFreezeSmallArray# m s of
  (# s', a #) -> (# s', HProduct a #)

-- | Create a new 'Struct' from a product.
thaw :: PrimMonad m => h :* xs -> m (Struct (PrimState m) h xs)
thaw (HProduct ar) = primitive $ \s -> case thawSmallArray# ar 0# (sizeofSmallArray# ar) s of
  (# s', m #) -> (# s', Struct m #)

hlength :: h :* xs -> Int
hlength (HProduct ar) = I# (sizeofSmallArray# ar)
{-# INLINE hlength #-}

unsafeMembership :: Int -> Membership xs x
unsafeMembership = unsafeCoerce#

hfoldrWithIndex :: (forall x. Membership xs x -> h x -> r -> r) -> r -> h :* xs -> r
hfoldrWithIndex f r p = foldr
  (\i -> let m = unsafeMembership i in f m (hlookup m p)) r [0..hlength p - 1]
{-# INLINE hfoldrWithIndex #-}

-- | Convert a product into an 'HList'.
toHList :: forall h xs. h :* xs -> L.HList h xs
toHList p = go 0 where
  go :: Int -> L.HList h xs
  go i
    | i == hlength p = unknownHList L.HNil
    | otherwise = unknownHList $ L.HCons (hlookup (unsafeMembership i) p) (go (i + 1))

  unknownHList :: L.HList h ys -> L.HList h zs
  unknownHList = unsafeCoerce#
{-# NOINLINE toHList #-}

-- | Create a new 'Struct' using the contents of a product.
newFrom :: forall g h m xs. (PrimMonad m)
  => g :* xs
  -> (forall x. Membership xs x -> g x -> h x)
  -> m (Struct (PrimState m) h xs)
newFrom hp@(HProduct ar) k = do
  st <- primitive $ \s -> case newSmallArray# (sizeofSmallArray# ar) undefined s of
    (# s', a #) -> (# s', Struct a #)
  forM_ [0..I# (sizeofSmallArray# ar) - 1]
    $ \i -> let m = unsafeMembership i in set st m $ k m (hlookup m hp)
  return st

hlookup :: Membership xs x -> h :* xs -> h x
hlookup (getMemberId -> I# i) (HProduct ar) = case indexSmallArray# ar i of
  (# a #) -> unsafeCoerce# a
{-# INLINE hlookup #-}

hfrozen :: (forall s. ST s (Struct s h xs)) -> h :* xs
hfrozen m = runST $ m >>= unsafeFreeze
{-# INLINE hfrozen #-}

instance (Corepresentable p, Comonad (Corep p), Functor f) => Extensible f p (:*) where
  -- | /O(log n)/ A lens for a value in a known position.
  pieceAt = pieceAt_
  {-# INLINE pieceAt #-}

pieceAt_ :: forall (xs :: [k]) (x :: k) (h :: k -> *) (f :: * -> *) (p :: * -> * -> *).
  (Functor f, Corepresentable p, Comonad (Corep p))
  => Membership xs x -> p (h x) (f (h x)) -> p (h :* xs) (f (h :* xs))
pieceAt_ i pafb = cotabulate $ \ws -> sbt (extract ws) <$> cosieve pafb (hlookup i <$> ws) where
  sbt xs x = hfrozen $ do
    s <- thaw xs
    set s i x
    return s
{-# INLINE pieceAt_ #-}
