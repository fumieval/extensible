{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Effect
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  TypeFamilies
--
-- Name-based extensible effects
-----------------------------------------------------------------------------
module Data.Extensible.Effect (
  -- * Base
  Instruction(..)
  , Eff
  , liftEff
  , hoistEff
  , handleEff
  , peelEff
  , leaveEff
  , retractEff
  , Handler(..)
  -- * Anonymous actions
  , Action(..)
  , Function
  , runAction
  , (@!?)
  -- * transformers-compatible handlers
  , runReaderAs
  , runStateAs
  , runWriterAs
  , runMaybeAs
  , runEitherAs
  ) where

import Control.Monad.Skeleton
import Control.Monad.Trans.State.Strict
import Data.Extensible.Field
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Extensible.Class
import Data.Profunctor.Unsafe -- Trustworthy since 7.8

-- | A unit of named effects.
data Instruction (xs :: [Assoc k (* -> *)]) a where
  Instruction :: !(Membership xs kv) -> AssocValue kv a -> Instruction xs a

-- | The extensible operational monad
type Eff xs = Skeleton (Instruction xs)

-- | Lift an instruction onto an 'Eff' action.
liftEff :: forall proxy s t xs a. Associate s t xs => proxy s -> t a -> Eff xs a
liftEff _ x = bone (Instruction (association :: Membership xs (s ':> t)) x)
{-# INLINE liftEff #-}

-- | Censor a specific type of effects in an action.
hoistEff :: forall proxy s t xs a. Associate s t xs => proxy s -> (forall x. t x -> t x) -> Eff xs a -> Eff xs a
hoistEff _ f = hoistSkeleton $ \(Instruction i t) -> case compareMembership (association :: Membership xs (s ':> t)) i of
  Right Refl -> Instruction i (f t)
  _ -> Instruction i t
{-# INLINABLE hoistEff #-}

-- | Build a relay-style handler from a triple of functions.
--
-- @
-- runStateAs = peelEff (\a s -> return (a, s))
--   (\m k s -> let (a, s') = runState m s in k a s')
--   (\i k s -> boned (i :>>= flip k s))
-- @
--
peelEff :: (a -> r) -- return the result
  -> (forall x. t x -> (x -> r) -> r) -- ^ Handle the foremost type of an action
  -> (forall x. Instruction xs x -> (x -> r) -> r) -- ^ Re-bind an unrelated action
  -> proxy k -> Eff (k >: t ': xs) a -> r
peelEff ret wrap pass _ = go where
  go m = case unbone m of
    Return a -> ret a
    Instruction i t :>>= k -> runMembership i
      (\Refl -> wrap t (go . k))
      (\j -> pass (Instruction j t) (go . k))
{-# INLINE peelEff #-}

-- | Reveal the final result of 'Eff'.
leaveEff :: Eff '[] a -> a
leaveEff m = case unbone m of
  Return a -> a
  _ -> error "Impossible"

-- | Tear down an action using the 'Monad' instance of the instruction.
retractEff :: Monad m => proxy k -> Eff '[k >: m] a -> m a
retractEff p m = case unbone m of
  Return a -> return a
  Instruction i t :>>= k -> runMembership i
    (\Refl -> t >>= retractEff p . k)
    (error "Impossible")

-- | Transformation between effects
newtype Handler f g = Handler { runHandler :: forall a. g a -> f a }

-- | Process an 'Eff' action using a record of 'Handler's.
handleEff :: RecordOf (Handler m) xs -> Eff xs a -> MonadView m (Eff xs) a
handleEff hs m = case unbone m of
  Instruction i t :>>= k -> views (pieceAt i) (runHandler .# getField) hs t :>>= k
  Return a -> Return a

-- | Name-agnostic representation of instructions.
data Action (args :: [*]) a r where
  AResult :: Action '[] a a
  AArgument :: x -> Action xs a r -> Action (x ': xs) a r

-- | @'Function' [a, b, c] r@ is @a -> b -> c -> r@
type family Function args r :: * where
  Function '[] r = r
  Function (x ': xs) r = x -> Function xs r

-- | Pass the arguments of 'Action' to the supplied function.
runAction :: Function xs (f a) -> Action xs a r -> f r
runAction r AResult = r
runAction f (AArgument x a) = runAction (f x) a

-- | Create a 'Field' of a 'Handler' for an 'Action'.
(@!?) :: FieldName k -> Function xs (f a) -> Field (Handler f) (k ':> Action xs a)
_ @!? f = Field $ Handler (runAction f)
infix 1 @!?

runReaderAs :: proxy k -> Eff (k >: (->) r ': xs) a -> r -> Eff xs a
runReaderAs = peelEff (\a _ -> return a)
  (\m k r -> k (m r) r)
  (\i k r -> boned (i :>>= flip k r))
{-# INLINE runReaderAs #-}

runStateAs :: proxy k -> Eff (k >: State s ': xs) a -> s -> Eff xs (a, s)
runStateAs = peelEff (\a s -> return (a, s))
  (\m k s -> let (a, s') = runState m s in k a s')
  (\i k s -> boned (i :>>= flip k s))
{-# INLINE runStateAs #-}

runWriterAs :: Monoid w => proxy k -> Eff (k >: (,) w ': xs) a -> Eff xs (a, w)
runWriterAs p m0 = peelEff (\a w -> return (a, w))
  (\(w', a) k w -> k a $! mappend w w')
  (\i k w -> boned (i :>>= flip k w)) p m0 mempty
{-# INLINE runWriterAs #-}

runMaybeAs :: proxy k -> Eff (k >: Maybe ': xs) a -> Eff xs (Maybe a)
runMaybeAs = peelEff (return . Just)
  (\m k -> maybe (return Nothing) k m)
  (\i k -> boned (i :>>= k))
{-# INLINE runMaybeAs #-}

runEitherAs :: proxy k -> Eff (k >: Either e ': xs) a -> Eff xs (Either e a)
runEitherAs = peelEff (return . Right)
  (\m k -> either (return . Left) k m)
  (\i k -> boned (i :>>= k))
{-# INLINE runEitherAs #-}
