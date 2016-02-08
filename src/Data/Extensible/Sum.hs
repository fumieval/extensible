{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Sum
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
------------------------------------------------------------------------
module Data.Extensible.Sum (
   (:|)(..)
  , hoist
  , embed
  , strike
  , strikeAt
  , (<:|)
  , exhaust
  , picked
  , embedAssoc
  ) where

import Data.Extensible.Internal
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Data.Typeable
import Data.Extensible.Class
import Data.Profunctor

-- | The extensible sum type
--
-- @(:|) :: (k -> *) -> [k] -> *@
--
data (h :: k -> *) :| (s :: [k]) where
  EmbedAt :: !(Membership xs x) -> h x -> h :| xs
deriving instance Typeable (:|)

instance Enum (Proxy :| xs) where
  fromEnum (EmbedAt m _) = fromIntegral $ getMemberId m
  toEnum i = reifyMembership (fromIntegral i) $ \m -> EmbedAt m Proxy

instance (Last xs ∈ xs) => Bounded (Proxy :| xs) where
  minBound = reifyMembership 0 $ \m -> EmbedAt m Proxy
  maxBound = EmbedAt (membership :: Membership xs (Last xs)) Proxy

-- | Change the wrapper.
hoist :: (forall x. g x -> h x) -> g :| xs -> h :| xs
hoist f (EmbedAt p h) = EmbedAt p (f h)
{-# INLINE hoist #-}

-- | /O(1)/ lift a value.
embed :: (x ∈ xs) => h x -> h :| xs
embed = EmbedAt membership
{-# INLINE embed #-}

-- | Try to extract something you want.
strike :: forall h x xs. (x ∈ xs) => h :| xs -> Maybe (h x)
strike = strikeAt membership
{-# INLINE strike #-}

-- | Try to extract something you want.
strikeAt :: forall h x xs. Membership xs x -> h :| xs -> Maybe (h x)
strikeAt q (EmbedAt p h) = case compareMembership p q of
  Right Refl -> Just h
  _ -> Nothing
{-# INLINE strikeAt #-}

-- | /O(1)/ Naive pattern match
(<:|) :: (h x -> r) -> (h :| xs -> r) -> h :| (x ': xs) -> r
(<:|) r c = \(EmbedAt i h) -> runMembership i
  (\Refl -> r h)
  (\j -> c (EmbedAt j h))
infixr 1 <:|
{-# INLINE (<:|) #-}

-- | There is no empty union.
exhaust :: h :| '[] -> r
exhaust _ = error "Impossible"

-- | Embed a value, but focuses on its key.
embedAssoc :: Associate k a xs => h (k ':> a) -> h :| xs
embedAssoc = EmbedAt association
{-# INLINE embedAssoc #-}

{-# DEPRECATED picked "Use piece instead" #-}
-- | A traversal that tries to point a specific element.
picked :: forall f h x xs. (x ∈ xs, Applicative f) => (h x -> f (h x)) -> h :| xs -> f (h :| xs)
picked f u@(EmbedAt i h) = case compareMembership (membership :: Membership xs x) i of
  Right Refl -> fmap (EmbedAt i) (f h)
  _ -> pure u
{-# INLINE picked #-}

instance (Applicative f, Choice p) => Extensible f p (:|) where
  pieceAt m = dimap (\t@(EmbedAt i h) -> case compareMembership i m of
    Right Refl -> Right h
    Left _ -> Left t) (either pure (fmap (EmbedAt m))) . right'
  {-# INLINABLE pieceAt #-}
