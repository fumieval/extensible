{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Class
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-----------------------------------------------------------------------------
module Data.Extensible.Class (
  -- * Class
   Extensible(..)
  , piece
  , pieceAssoc
  , itemAt
  , item
  , itemAssoc
  -- * Membership
  , Membership
  , mkMembership
  , getMemberId
  , compareMembership
  , leadership
  -- * Member
  , Member(..)
  , type (∈)
  , FindType
  -- * Generation
  , Generate(..)
  , Forall(..)
  , ForallF
  -- * Association
  , Assoc(..)
  , type (>:)
  , Lookup(..)
  , Associate
  -- * Misc
  , Head
  , Last
  ) where
import Data.Constraint
import Data.Extensible.Internal.Rig (Optic')
import Data.Extensible.Wrapper
import Data.Profunctor
import Type.Membership
import Type.Membership.Internal

-- | This class allows us to use 'pieceAt' for both sums and products.
class (Functor f, Profunctor p) => Extensible f p (t :: [k] -> (k -> *) -> *) where
  type ExtensibleConstr t (xs :: [k]) (h :: k -> *) (x :: k) :: Constraint
  type ExtensibleConstr t xs h x = ()
  pieceAt :: ExtensibleConstr t xs h x => Membership xs x -> Optic' p f (t xs h) (h x)

-- | Accessor for an element.
piece :: (x ∈ xs, Extensible f p t, ExtensibleConstr t xs h x) => Optic' p f (t xs h) (h x)
piece = pieceAt membership
{-# INLINE piece #-}

-- | Like 'piece', but reckon membership from its key.
pieceAssoc :: (Lookup xs k v, Extensible f p t, ExtensibleConstr t xs h (k ':> v)) => Optic' p f (t xs h) (h (k ':> v))
pieceAssoc = pieceAt association
{-# INLINE pieceAssoc #-}

-- | Access a specified element through a wrapper.
itemAt :: (Wrapper h, Extensible f p t, ExtensibleConstr t xs h x) => Membership xs x -> Optic' p f (t xs h) (Repr h x)
itemAt m = pieceAt m . _Wrapper
{-# INLINE itemAt #-}

-- | Access an element through a wrapper.
item :: (Wrapper h, Extensible f p t, x ∈ xs, ExtensibleConstr t xs h x) => proxy x -> Optic' p f (t xs h) (Repr h x)
item p = piece . _WrapperAs p
{-# INLINE item #-}

-- | Access an element specified by the key type through a wrapper.
itemAssoc :: (Wrapper h, Extensible f p t, Lookup xs k v, ExtensibleConstr t xs h (k ':> v))
  => proxy k -> Optic' p f (t xs h) (Repr h (k ':> v))
itemAssoc p = pieceAssoc . _WrapperAs (proxyKey p)
{-# INLINE itemAssoc #-}

proxyKey :: proxy k -> Proxy (k ':> v)
proxyKey _ = Proxy
{-# INLINE proxyKey #-}

-- | First element
type family Head (xs :: [k]) :: k where
  Head (x ': xs) = x

-- | Last element
type family Last (x :: [k]) :: k where
  Last '[x] = x
  Last (x ': xs) = Last xs

type Associate k v xs = Lookup xs k v
{-# DEPRECATED Associate "Use Lookup instead" #-}
