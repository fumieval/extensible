{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Sum
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
------------------------------------------------------------------------
module Data.Extensible.Sum (
  (:/)(..)
  , hoist
  , embed
  , strike
  , strikeAt
  , (<:|)
  , exhaust
  , embedAssoc
  ) where

import Data.Extensible.Class
import Data.Kind (Type)
import Data.Profunctor
import Data.Proxy
import Data.Type.Equality
import Type.Membership

-- | The extensible sum type
--
-- @(:/) :: [k] -> (k -> Type) -> Type@
--
data (xs :: [k]) :/ (h :: k -> Type) where
  EmbedAt :: !(Membership xs x) -> h x -> xs :/ h

instance Enum (xs :/ Proxy) where
  fromEnum (EmbedAt m _) = fromIntegral $ getMemberId m
  toEnum i = reifyMembership (fromIntegral i) $ \m -> EmbedAt m Proxy

instance (Last xs ∈ xs) => Bounded (xs :/ Proxy) where
  minBound = reifyMembership 0 $ \m -> EmbedAt m Proxy
  maxBound = EmbedAt (membership :: Membership xs (Last xs)) Proxy

-- | Change the wrapper.
hoist :: (forall x. g x -> h x) -> xs :/ g -> xs :/ h
hoist f (EmbedAt p h) = EmbedAt p (f h)
{-# INLINE hoist #-}

-- | /O(1)/ lift a value.
embed :: (x ∈ xs) => h x -> xs :/ h
embed = EmbedAt membership
{-# INLINE embed #-}

-- | Try to extract something you want.
strike :: forall h x xs. (x ∈ xs) => xs :/ h -> Maybe (h x)
strike = strikeAt membership
{-# INLINE strike #-}

-- | Try to extract something you want.
strikeAt :: forall h x xs. Membership xs x -> xs :/ h -> Maybe (h x)
strikeAt q (EmbedAt p h) = case compareMembership p q of
  Right Refl -> Just h
  _ -> Nothing
{-# INLINE strikeAt #-}

-- | /O(1)/ Naive pattern match
(<:|) :: (h x -> r)
    -> (xs :/ h -> r)
    -> (x ': xs) :/ h
    -> r
(<:|) r c = \(EmbedAt i h) -> testMembership i
  (\Refl -> r h)
  (\j -> c (EmbedAt j h))
infixr 1 <:|
{-# INLINE (<:|) #-}

-- | There is no empty union.
exhaust :: '[] :/ h -> r
exhaust _ = error "Impossible"

-- | Embed a value, but focuses on its key.
embedAssoc :: Lookup xs k a => h (k ':> a) -> xs :/ h
embedAssoc = EmbedAt association
{-# INLINE embedAssoc #-}

instance (Applicative f, Choice p) => Extensible f p (:/) where
  pieceAt m = dimap (\t@(EmbedAt i h) -> case compareMembership i m of
    Right Refl -> Right h
    Left _ -> Left t) (either pure (fmap (EmbedAt m))) . right'
  {-# INLINABLE pieceAt #-}
