{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns, BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Struct
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Mutable structs
------------------------------------------------------------------------
module Data.Extensible.Struct (
  -- * Mutable struct
  Struct
  , set
  , get
  , new
  , newRepeat
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
  , hmodify
  , toHList) where

import GHC.Prim
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Constraint
import Data.Extensible.Class
import Data.Extensible.Internal
import Control.Comonad
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import qualified Data.Extensible.HList as L
import GHC.Types

-- | Mutable type-indexed struct.
data Struct s (h :: k -> *) (xs :: [k]) = Struct (SmallMutableArray# s Any)

-- | Write a value in a 'Struct'.
set :: PrimMonad m => Struct (PrimState m) h xs -> Membership xs x -> h x -> m ()
set (Struct m) (getMemberId -> I# i) e = primitive
  $ \s -> case unsafeCoerce# writeSmallArray# m i e s of
    s' -> (# s', () #)
{-# INLINE set #-}

-- | Read a value from a 'Struct'.
get :: PrimMonad m => Struct (PrimState m) h xs -> Membership xs x -> m (h x)
get (Struct m) (getMemberId -> I# i) = primitive $ unsafeCoerce# readSmallArray# m i
{-# INLINE get #-}

-- | Create a new 'Struct' using the supplied initializer.
new :: forall h m xs. (PrimMonad m, Generate xs)
  => (forall x. Membership xs x -> h x)
  -> m (Struct (PrimState m) h xs)
new = newDict Dict
{-# INLINE new #-}

newDict :: PrimMonad m
  => Dict (Generate xs)
  -> (forall x. Membership xs x -> h x)
  -> m (Struct (PrimState m) h xs)
newDict Dict k = do
  m <- newRepeat undefined
  henumerate (\i cont -> set m i (k i) >> cont) $ return m
{-# NOINLINE[0] newDict #-}

-- | Create a 'Struct' full of the specified value.
newRepeat :: forall h m xs. (PrimMonad m, Generate xs)
  => (forall x. h x)
  -> m (Struct (PrimState m) h xs)
newRepeat x = do
  let !(I# n) = hcount (Proxy :: Proxy xs)
  primitive $ \s -> case newSmallArray# n (unsafeCoerce# x) s of
    (# s', a #) -> (# s', Struct a #)
{-# INLINE newRepeat #-}

-- | Create a new 'Struct' using the supplied initializer with a context.
newFor :: forall proxy c h m xs. (PrimMonad m, Forall c xs)
  => proxy c
  -> (forall x. c x => Membership xs x -> h x)
  -> m (Struct (PrimState m) h xs)
newFor = newForDict Dict
{-# INLINE newFor #-}

newForDict :: forall proxy c h m xs. (PrimMonad m)
  => Dict (Forall c xs)
  -> proxy c
  -> (forall x. c x => Membership xs x -> h x)
  -> m (Struct (PrimState m) h xs)
newForDict Dict p k = do
  m <- newRepeat undefined
  henumerateFor p (Proxy :: Proxy xs) (\i cont -> set m i (k i) >> cont) $ return m
{-# NOINLINE[0] newForDict #-}

-- | Create a new 'Struct' from an 'HList'.
newFromHList :: forall h m xs. PrimMonad m => L.HList h xs -> m (Struct (PrimState m) h xs)
newFromHList l = do
  let !(I# size) = L.hlength l
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
{-# INLINE unsafeFreeze #-}

-- | Create a new 'Struct' from a product.
thaw :: PrimMonad m => h :* xs -> m (Struct (PrimState m) h xs)
thaw (HProduct ar) = primitive $ \s -> case thawSmallArray# ar 0# (sizeofSmallArray# ar) s of
  (# s', m #) -> (# s', Struct m #)

-- | The size of a product.
hlength :: h :* xs -> Int
hlength (HProduct ar) = I# (sizeofSmallArray# ar)
{-# INLINE hlength #-}

unsafeMembership :: Int -> Membership xs x
unsafeMembership = unsafeCoerce#

-- | Right-associative fold of a product.
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
  let !n = sizeofSmallArray# ar
  st <- primitive $ \s -> case newSmallArray# n undefined s of
    (# s', a #) -> (# s', Struct a #)
  let go i
        | i == I# n = return st
        | otherwise = do
          let !m = unsafeMembership i
          set st m $ k m (hlookup m hp)
          go (i + 1)
  go 0
{-# NOINLINE newFrom #-}

{-# RULES "newFrom/newFrom" forall p (f :: forall x. Membership xs x -> f x -> g x)
 (g :: forall x. Membership xs x -> g x -> h x)
  . newFrom (hfrozen (newFrom p f)) g = newFrom p (\i x -> g i (f i x)) #-}

{-# RULES "newFrom/newDict" forall d (f :: forall x. Membership xs x -> g x)
 (g :: forall x. Membership xs x -> g x -> h x)
  . newFrom (hfrozen (newDict d f)) g = newDict d (\i -> g i (f i)) #-}

{-# RULES "newFrom/newForDict" forall d p (f :: forall x. Membership xs x -> g x)
 (g :: forall x. Membership xs x -> g x -> h x)
  . newFrom (hfrozen (newForDict d p f)) g = newForDict d p (\i -> g i (f i)) #-}

-- | Get an element in a product.
hlookup :: Membership xs x -> h :* xs -> h x
hlookup (getMemberId -> I# i) (HProduct ar) = case indexSmallArray# ar i of
  (# a #) -> unsafeCoerce# a
{-# INLINE hlookup #-}

-- | Create a product from an 'ST' action which returns a 'Struct'.
hfrozen :: (forall s. ST s (Struct s h xs)) -> h :* xs
hfrozen m = runST $ m >>= unsafeFreeze
{-# INLINE[0] hfrozen #-}

hmodify :: (forall s. Struct s h xs -> ST s ()) -> h :* xs -> h :* xs
hmodify f m = runST $ do
  s <- thaw m
  f s
  unsafeFreeze s
{-# INLINE[0] hmodify #-}

{-# RULES "hmodify/batch" forall
  (a :: forall s. Struct s h xs -> ST s ())
  (b :: forall s. Struct s h xs -> ST s ())
  (x :: h :* xs). hmodify b (hmodify a x) = hmodify (\s -> a s >> b s) x  #-}

instance (Corepresentable p, Comonad (Corep p), Functor f) => Extensible f p (:*) where
  -- | A lens for a value in a known position.
  pieceAt i pafb = cotabulate $ \ws -> sbt (extract ws) <$> cosieve pafb (hlookup i <$> ws) where
    sbt xs x = hmodify (\s -> set s i x) xs
  {-# INLINE pieceAt #-}
