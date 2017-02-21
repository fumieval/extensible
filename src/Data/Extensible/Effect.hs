{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Effect
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Name-based extensible effects
-----------------------------------------------------------------------------
module Data.Extensible.Effect (
  -- * Base
  Instruction(..)
  , Eff
  , liftEff
  , liftsEff
  , hoistEff
  -- * Step-wise handling
  , Handler(..)
  , handleEff
  -- * Peeling
  , peelEff
  , rebindEff0
  , rebindEff1
  , rebindEff2
  , leaveEff
  , retractEff
  -- * Anonymous actions
  , Action(..)
  , Function
  , runAction
  , (@!?)
  , peelAction
  -- * transformers-compatible actions and handlers
  -- ** Reader
  , ReaderEff
  , askEff
  , asksEff
  , localEff
  , runReaderEff
  -- ** State
  , State
  , getEff
  , getsEff
  , putEff
  , modifyEff
  , stateEff
  , runStateEff
  -- ** Writer
  , WriterEff
  , writerEff
  , tellEff
  , listenEff
  , passEff
  , runWriterEff
  -- ** Maybe
  , MaybeEff
  , runMaybeEff
  -- ** Either
  , EitherEff
  , throwEff
  , catchEff
  , runEitherEff
  -- ** Iter
  , Identity
  , tickEff
  , runIterEff
  ) where

import Control.Applicative
import Control.Monad.Skeleton
import Control.Monad.Trans.State.Strict
import Data.Extensible.Field
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Extensible.Class
import Data.Functor.Identity
import Data.Profunctor.Unsafe -- Trustworthy since 7.8

-- | A unit of named effects.
data Instruction (xs :: [Assoc k (* -> *)]) a where
  Instruction :: !(Membership xs kv) -> AssocValue kv a -> Instruction xs a

-- | The extensible operational monad
type Eff xs = Skeleton (Instruction xs)

-- | Lift an instruction onto an 'Eff' action.
liftEff :: forall s t xs a proxy. Associate s t xs => proxy s -> t a -> Eff xs a
liftEff p x = liftsEff p x id
{-# INLINE liftEff #-}

-- | Lift an instruction onto an 'Eff' action and apply a function to the result.
liftsEff :: forall s t xs a r proxy. Associate s t xs
  => proxy s -> t a -> (a -> r) -> Eff xs r
liftsEff _ x k = boned
  $ Instruction (association :: Membership xs (s ':> t)) x :>>= return . k
{-# INLINE liftsEff #-}

-- | Censor a specific type of effects in an action.
hoistEff :: forall s t xs a proxy. Associate s t xs => proxy s -> (forall x. t x -> t x) -> Eff xs a -> Eff xs a
hoistEff _ f = hoistSkeleton $ \(Instruction i t) -> case compareMembership (association :: Membership xs (s ':> t)) i of
  Right Refl -> Instruction i (f t)
  _ -> Instruction i t
{-# INLINABLE hoistEff #-}

-- | Build a relay-style handler from a triple of functions.
--
-- @
-- runStateEff = peelEff rebindEff1 (\a s -> return (a, s))
--   (\m k s -> let (a, s') = runState m s in k a s')
-- @
--
peelEff :: forall k t xs a r
  . (forall x. Instruction xs x -> (x -> r) -> r) -- ^ Re-bind an unrelated action
  -> (a -> r) -- ^ return the result
  -> (forall x. t x -> (x -> r) -> r) -- ^ Handle the foremost type of an action
  -> Eff (k >: t ': xs) a -> r
peelEff pass ret wrap = go where
  go m = case unbone m of
    Return a -> ret a
    Instruction i t :>>= k -> runMembership i
      (\Refl -> wrap t (go . k))
      (\j -> pass (Instruction j t) (go . k))
{-# INLINE peelEff #-}

-- | A common value for the second argument of 'peelEff'. Binds an instruction
-- directly.
rebindEff0 :: Instruction xs x -> (x -> Eff xs a) -> Eff xs a
rebindEff0 i k = boned (i :>>= k)

-- | A pre-defined value for the second argument of 'peelEff'.
-- Preserves the argument of the continuation.
rebindEff1 :: Instruction xs x -> (x -> r -> Eff xs a) -> r -> Eff xs a
rebindEff1 i k r = boned (i :>>= flip k r)

-- | A pre-defined value for the second argument of 'peelEff'.
-- Preserves two arguments of the continuation.
rebindEff2 :: Instruction xs x -> (x -> r -> s -> Eff xs a) -> r -> s -> Eff xs a
rebindEff2 i k r s = boned (i :>>= \x -> k x r s)

-- | Reveal the final result of 'Eff'.
leaveEff :: Eff '[] a -> a
leaveEff m = case unbone m of
  Return a -> a
  _ -> error "Impossible"

-- | Tear down an action using the 'Monad' instance of the instruction.
retractEff :: forall k m a. Monad m => Eff '[k >: m] a -> m a
retractEff m = case unbone m of
  Return a -> return a
  Instruction i t :>>= k -> runMembership i
    (\Refl -> t >>= retractEff . k)
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

-- | Specialised version of 'peelEff' for 'Action's.
-- You can pass a function @a -> b -> ... -> (q -> r) -> r@ as a handler for
-- @'Action' '[a, b, ...] q@.
peelAction :: forall k ps q xs a r
  . (forall x. Instruction xs x -> (x -> r) -> r) -- ^ Re-bind an unrelated action
  -> (a -> r) -- ^ return the result
  -> Function ps ((q -> r) -> r) -- ^ Handle the foremost action
  -> Eff (k >: Action ps q ': xs) a -> r
peelAction pass ret wrap = go where
  go m = case unbone m of
    Return a -> ret a
    Instruction i t :>>= k -> runMembership i
      (\Refl -> case t of
        (_ :: Action ps q x) ->
          let run :: forall t. Function t ((q -> r) -> r) -> Action t q x -> r
              run f AResult = f (go . k)
              run f (AArgument x a) = run (f x) a
          in run wrap t)
      (\j -> pass (Instruction j t) (go . k))
{-# INLINE peelAction #-}

-- | The reader monad is characterised by an action with a type equality between
-- the result type and the enviroment type.
type ReaderEff r = (:~:) r

-- | Fetch the environment.
askEff :: forall k r xs proxy. Associate k (ReaderEff r) xs
  => proxy k -> Eff xs r
askEff p = liftEff p Refl
{-# INLINE askEff #-}

-- | Pass the environment to a function.
asksEff :: forall k r xs a proxy. Associate k (ReaderEff r) xs
  => proxy k -> (r -> a) -> Eff xs a
asksEff p = liftsEff p Refl
{-# INLINE asksEff #-}

-- | Modify the enviroment locally.
localEff :: forall k r xs a proxy. Associate k (ReaderEff r) xs
  => proxy k -> (r -> r) -> Eff xs a -> Eff xs a
localEff _ f = go where
  go m = case unbone m of
    Return a -> return a
    Instruction i t :>>= k -> case compareMembership
      (association :: Membership xs (k >: ReaderEff r)) i of
        Left _ -> boned $ Instruction i t :>>= go . k
        Right Refl -> case t of
          Refl -> boned $ Instruction i t :>>= go . k . f
{-# INLINE localEff #-}

-- | Run the frontal reader effect.
runReaderEff :: forall k r xs a. Eff (k >: ReaderEff r ': xs) a -> r -> Eff xs a
runReaderEff = peelEff rebindEff1 (\a _ -> return a)
  (\Refl k r -> k r r)
{-# INLINE runReaderEff #-}

-- | Get the current state.
getEff :: forall k s xs proxy. Associate k (State s) xs
  => proxy k -> Eff xs s
getEff k = liftEff k get
{-# INLINE getEff #-}

-- | Pass the current state to a function.
getsEff :: forall k s a xs proxy. Associate k (State s) xs
  => proxy k -> (s -> a) -> Eff xs a
getsEff k = liftsEff k get
{-# INLINE getsEff #-}

-- | Replace the state with a new value.
putEff :: forall k s xs proxy. Associate k (State s) xs
  => proxy k -> s -> Eff xs ()
putEff k = liftEff k . put
{-# INLINE putEff #-}

-- | Modify the state.
modifyEff :: forall k s xs proxy. Associate k (State s) xs
  => proxy k -> (s -> s) -> Eff xs ()
modifyEff k f = liftEff k $ state $ \s -> ((), f s)
{-# INLINE modifyEff #-}

-- | Lift a state modification function.
stateEff :: forall k s xs a proxy. Associate k (State s) xs
  => proxy k -> (s -> (a, s)) -> Eff xs a
stateEff k = liftEff k . state
{-# INLINE stateEff #-}

-- | Run the frontal state effect.
runStateEff :: forall k s xs a. Eff (k >: State s ': xs) a -> s -> Eff xs (a, s)
runStateEff = peelEff rebindEff1 (\a s -> return (a, s))
  (\m k s -> let (a, s') = runState m s in k a $! s')
{-# INLINE runStateEff #-}

-- | Writer
type WriterEff w = (,) w

-- | Write the second element and return the first element.
writerEff :: forall k w xs a proxy. (Associate k (WriterEff w) xs)
  => proxy k -> (a, w) -> Eff xs a
writerEff k (a, w) = liftEff k (w, a)
{-# INLINE writerEff #-}

-- | Write a value.
tellEff :: forall k w xs proxy. (Associate k (WriterEff w) xs)
  => proxy k -> w -> Eff xs ()
tellEff k w = liftEff k (w, ())
{-# INLINE tellEff #-}

-- | Squash the outputs into one step and return it.
listenEff :: forall k w xs a proxy. (Associate k (WriterEff w) xs, Monoid w)
  => proxy k -> Eff xs a -> Eff xs (a, w)
listenEff p = go mempty where
  go w m = case unbone m of
    Return a -> writerEff p ((a, w), w)
    Instruction i t :>>= k -> case compareMembership (association :: Membership xs (k ':> (,) w)) i of
      Left _ -> boned $ Instruction i t :>>= go w . k
      Right Refl -> let (w', a) = t
                        !w'' = mappend w w' in go w'' (k a)
{-# INLINE listenEff #-}

-- | Modify the output using the function in the result.
passEff :: forall k w xs a proxy. (Associate k (WriterEff w) xs, Monoid w)
  => proxy k -> Eff xs (a, w -> w) -> Eff xs a
passEff p = go mempty where
  go w m = case unbone m of
    Return (a, f) -> writerEff p (a, f w)
    Instruction i t :>>= k -> case compareMembership (association :: Membership xs (k ':> (,) w)) i of
      Left _ -> boned $ Instruction i t :>>= go w . k
      Right Refl -> let (w', a) = t
                        !w'' = mappend w w' in go w'' (k a)
{-# INLINE passEff #-}

-- | Run the frontal writer effect.
runWriterEff :: forall k w xs a. Monoid w => Eff (k >: WriterEff w ': xs) a -> Eff xs (a, w)
runWriterEff = peelEff rebindEff1 (\a w -> return (a, w))
  (\(w', a) k w -> k a $! mappend w w') `flip` mempty
{-# INLINE runWriterEff #-}

-- | An effect with no result
type MaybeEff = Const ()

-- | Run an effect which may fail in the name of @k@.
runMaybeEff :: forall k xs a. Eff (k >: MaybeEff ': xs) a -> Eff xs (Maybe a)
runMaybeEff = peelEff rebindEff0 (return . Just)
  (\_ _ -> return Nothing)
{-# INLINE runMaybeEff #-}

-- | Throwing an exception
type EitherEff = Const

-- | Throw an exception @e@, throwing the rest of the computation away.
throwEff :: forall k e xs a proxy. (Associate k (EitherEff e) xs)
  => proxy k -> e -> Eff xs a
throwEff k = liftEff k . Const
{-# INLINE throwEff #-}

-- | Attach a handler for an exception.
catchEff :: forall k e xs a proxy. (Associate k (EitherEff e) xs)
  => proxy k -> Eff xs a -> (e -> Eff xs a) -> Eff xs a
catchEff _ m0 handler = go m0 where
  go m = case unbone m of
    Return a -> return a
    Instruction i t :>>= k -> case compareMembership (association :: Membership xs (k ':> Const e)) i of
      Left _ -> boned $ Instruction i t :>>= go . k
      Right Refl -> handler (getConst t)
{-# INLINE catchEff #-}

-- | Attach the frontal Either effect.
runEitherEff :: forall k e xs a. Eff (k >: EitherEff e ': xs) a -> Eff xs (Either e a)
runEitherEff = peelEff rebindEff0 (return . Right)
  (\(Const e) _ -> return $ Left e)
{-# INLINE runEitherEff #-}

-- | Put a milestone on a computation.
tickEff :: Associate k Identity xs => proxy k -> Eff xs ()
tickEff k = liftEff k (Identity ())
{-# INLINE tickEff #-}

-- | Run a computation until 'tickEff'.
runIterEff :: Eff (k >: Identity ': xs) a
  -> Eff xs (Either a (Eff (k >: Identity ': xs) a))
runIterEff m = case unbone m of
  Return a -> return (Left a)
  Instruction i t :>>= k -> runMembership i
    (\Refl -> return $ Right $ k $ runIdentity t)
    (\j -> boned $ Instruction j t :>>= runIterEff . k)
