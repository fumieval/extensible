{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
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
  , pattern UnionAt
  ) where

import Data.Extensible.Internal
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Data.Typeable

-- | The extensible sum type
data (h :: k -> *) :| (s :: [k]) where
  EmbedAt :: !(Membership xs x) -> h x -> h :| xs
deriving instance Typeable (:|)

{-# DEPRECATED UnionAt "This has renamed to EmbedAt" #-}
pattern UnionAt a b = EmbedAt a b

-- | Change the wrapper.
hoist :: (forall x. g x -> h x) -> g :| xs -> h :| xs
hoist f (EmbedAt p h) = EmbedAt p (f h)
{-# INLINE hoist #-}

-- | /O(1)/ lift a value.
embed :: (x ∈ xs) => h x -> h :| xs
embed = EmbedAt membership
{-# INLINE embed #-}

strike :: forall h x xs. (x ∈ xs) => h :| xs -> Maybe (h x)
strike = strikeAt membership
{-# INLINE strike #-}

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

-- | A traversal that tries to point a specific element.
picked :: forall f h x xs. (x ∈ xs, Applicative f) => (h x -> f (h x)) -> h :| xs -> f (h :| xs)
picked f u@(EmbedAt i h) = case compareMembership (membership :: Membership xs x) i of
  Right Refl -> fmap (EmbedAt i) (f h)
  _ -> pure u
{-# INLINE picked #-}

embedAssoc :: Associate k a xs => h (k ':> a) -> h :| xs
embedAssoc = EmbedAt association
{-# INLINE embedAssoc #-}
