{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Data.Extensible.Effect (Instruction(..)
  , Eff
  , liftEff
  , hoistEff
  , handleWith
  , Handler(..)
  -- * Unnamed actions
  , Action(..)
  , Function
  , receive) where

import Control.Monad.Skeleton
import Data.Extensible.Field
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Extensible.Class
import Data.Profunctor.Unsafe -- Trustworthy since 7.8

-- | Unnamed action
data Action (args :: [*]) a r where
  AResult :: Action '[] a a
  AArgument :: x -> Action xs a r -> Action (x ': xs) a r

type family Function args r :: * where
  Function '[] r = r
  Function (x ': xs) r = x -> Function xs r

-- | Transformation between effects
newtype Handler f g = Handler { runHandler :: forall a. g a -> f a }

receive :: Functor f => Function xs (f a) -> Handler f (Action xs a)
receive f0 = Handler (go f0) where
  go :: Functor f => Function xs (f a) -> Action xs a r -> f r
  go r AResult = r
  go f (AArgument x a) = go (f x) a

----------------------------------------------

-- | A unit of effects
data Instruction (xs :: [Assoc k (* -> *)]) a where
  Instruction :: !(Membership xs kv) -> AssocValue kv a -> Instruction xs a

-- | The extensible operational monad
type Eff xs = Skeleton (Instruction xs)

-- | Lift some effect to 'Eff'
liftEff :: forall proxy s t xs a. Associate s t xs => proxy s -> t a -> Eff xs a
liftEff _ x = bone (Instruction (association :: Membership xs (s ':> t)) x)
{-# INLINE liftEff #-}

hoistEff :: forall proxy s t xs a. Associate s t xs => proxy s -> (forall x. t x -> t x) -> Eff xs a -> Eff xs a
hoistEff _ f = hoistSkeleton $ \(Instruction i t) -> case compareMembership (association :: Membership xs (s ':> t)) i of
  Right Refl -> Instruction i (f t)
  _ -> Instruction i t
{-# INLINABLE hoistEff #-}

handleWith :: RecordOf (Handler m) xs -> Eff xs a -> MonadView m (Eff xs) a
handleWith hs m = case unbone m of
  Instruction i t :>>= k -> views (pieceAt i) (runHandler .# getField) hs t :>>= k
  Return a -> Return a
{-# INLINABLE handleWith #-}
