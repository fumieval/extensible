{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Effect
-- Copyright   :  (c) Fumiaki Kinoshita 2018
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
  , castEff
  -- * Step-wise handling
  , Interpreter(..)
  , handleEff
  -- * Peeling
  , peelEff
  , Rebinder
  , rebindEff0
  , peelEff0
  , rebindEff1
  , peelEff1
  , rebindEff2
  , leaveEff
  , retractEff
  -- * Anonymous actions
  , Action(..)
  , Function
  , runAction
  , (@!?)
  , peelAction
  , peelAction0
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
  , execStateEff
  , evalStateEff
  -- ** Writer
  , WriterEff
  , writerEff
  , tellEff
  , listenEff
  , passEff
  , runWriterEff
  , execWriterEff
  -- ** Maybe
  , MaybeEff
  , nothingEff
  , runMaybeEff
  -- ** Either
  , EitherEff
  , throwEff
  , catchEff
  , runEitherEff
  , mapLeftEff
  -- ** Iter
  , Identity
  , tickEff
  , runIterEff
  -- ** Cont
  , ContT
  , contEff
  , runContEff
  , callCCEff
  ) where

import Control.Applicative
import Data.Bifunctor (first)
import Control.Monad.Skeleton
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Cont (ContT(..))
import Data.Extensible.Field
import Data.Extensible.Inclusion
import Data.Extensible.Internal.Rig
import Data.Extensible.Product
import Data.Extensible.Class
import Data.Functor.Identity
import Data.Profunctor.Unsafe -- Trustworthy since 7.8
import Data.Type.Equality
import Type.Membership

-- | A unit of named effects. This is a variant of @(':|')@ specialised for
-- 'Type -> Type'.
data Instruction (xs :: [Assoc k (* -> *)]) a where
  Instruction :: !(Membership xs kv) -> TargetOf kv a -> Instruction xs a

-- | The extensible operational monad
type Eff xs = Skeleton (Instruction xs)

-- | Lift an instruction onto an 'Eff' action.
liftEff :: forall s t xs a. Lookup xs s t => Proxy s -> t a -> Eff xs a
liftEff p x = liftsEff p x id
{-# INLINE liftEff #-}

-- | Lift an instruction onto an 'Eff' action and apply a function to the result.
liftsEff :: forall s t xs a r. Lookup xs s t
  => Proxy s -> t a -> (a -> r) -> Eff xs r
liftsEff _ x k = boned
  $ Instruction (association :: Membership xs (s ':> t)) x :>>= return . k
{-# INLINE liftsEff #-}

-- | Censor a specific type of effects in an action.
hoistEff :: forall s t xs a. Lookup xs s t => Proxy s -> (forall x. t x -> t x) -> Eff xs a -> Eff xs a
hoistEff _ f = hoistSkeleton $ \(Instruction i t) -> case compareMembership (association :: Membership xs (s ':> t)) i of
  Right Refl -> Instruction i (f t)
  _ -> Instruction i t
{-# INLINABLE hoistEff #-}

-- | Upcast an action.
castEff :: IncludeAssoc ys xs => Eff xs a -> Eff ys a
castEff = hoistSkeleton
  $ \(Instruction i t) -> Instruction (hlookup i inclusionAssoc) t

-- | Build a relay-style handler from a triple of functions.
--
-- @
-- runStateEff = peelEff1 (\a s -> return (a, s))
--   (\m k s -> let (a, s') = runState m s in k a s')
-- @
--
peelEff :: forall k t xs a r
  . Rebinder xs r -- ^ Re-bind an unrelated action
  -> (a -> r) -- ^ return the result
  -> (forall x. t x -> (x -> r) -> r) -- ^ Handle the foremost type of an action
  -> Eff (k >: t ': xs) a -> r
peelEff pass ret wrap = go where
  go m = case debone m of
    Return a -> ret a
    Instruction i t :>>= k -> testMembership i
      (\Refl -> wrap t (go . k))
      (\j -> pass (Instruction j t) (go . k))
{-# INLINE peelEff #-}

-- | 'peelEff' specialised for continuations with no argument
peelEff0 :: forall k t xs a r. (a -> Eff xs r) -- ^ return the result
  -> (forall x. t x -> (x -> Eff xs r) -> Eff xs r) -- ^ Handle the foremost type of an action
  -> Eff (k >: t ': xs) a -> Eff xs r
peelEff0 = peelEff rebindEff0
{-# INLINE peelEff0 #-}

-- | 'peelEff' specialised for 1-argument continuation
peelEff1 :: forall k t xs a b r. (a -> b -> Eff xs r) -- ^ return the result
  -> (forall x. t x -> (x -> b -> Eff xs r) -> b -> Eff xs r) -- ^ Handle the foremost type of an action
  -> Eff (k >: t ': xs) a -> b -> Eff xs r
peelEff1 = peelEff rebindEff1
{-# INLINE peelEff1 #-}

-- | A function to bind an 'Instruction' in 'peelEff'.
type Rebinder xs r = forall x. Instruction xs x -> (x -> r) -> r

-- | A common value for the second argument of 'peelEff'. Binds an instruction
-- directly.
rebindEff0 :: Rebinder xs (Eff xs r)
rebindEff0 i k = boned $ i :>>= k

-- | A pre-defined value for the second argument of 'peelEff'.
-- Preserves the argument of the continuation.
rebindEff1 :: Rebinder xs (a -> Eff xs r)
rebindEff1 i k a = boned $ i :>>= flip k a

-- | A pre-defined value for the second argument of 'peelEff'.
-- Preserves two arguments of the continuation.
rebindEff2 :: Rebinder xs (a -> b -> Eff xs r)
rebindEff2 i k a b = boned $ i :>>= \x -> k x a b

-- | Reveal the final result of 'Eff'.
leaveEff :: Eff '[] a -> a
leaveEff m = case debone m of
  Return a -> a
  _ -> error "Impossible"

-- | Tear down an action using the 'Monad' instance of the instruction.
retractEff :: forall k m a. Monad m => Eff '[k >: m] a -> m a
retractEff m = case debone m of
  Return a -> return a
  Instruction i t :>>= k -> testMembership i
    (\Refl -> t >>= retractEff . k)
    $ error "Impossible"

-- | Transformation between effects
newtype Interpreter f g = Interpreter { runInterpreter :: forall a. g a -> f a }

-- | Process an 'Eff' action using a record of 'Interpreter's.
handleEff :: RecordOf (Interpreter m) xs -> Eff xs a -> MonadView m (Eff xs) a
handleEff hs m = case debone m of
  Instruction i t :>>= k -> views (pieceAt i) (runInterpreter .# getField) hs t :>>= k
  Return a -> Return a

-- | Anonymous representation of instructions.
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

-- | Create a 'Field' of a 'Interpreter' for an 'Action'.
(@!?) :: FieldName k -> Function xs (f a) -> Field (Interpreter f) (k ':> Action xs a)
_ @!? f = Field $ Interpreter $ runAction f
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
  go m = case debone m of
    Return a -> ret a
    Instruction i t :>>= k -> testMembership i
      (\Refl -> case t of
        (_ :: Action ps q x) ->
          let run :: forall t. Function t ((q -> r) -> r) -> Action t q x -> r
              run f AResult = f (go . k)
              run f (AArgument x a) = run (f x) a
          in run wrap t)
      $ \j -> pass (Instruction j t) (go . k)
{-# INLINE peelAction #-}

-- | Non continuation-passing variant of 'peelAction'.
peelAction0 :: forall k ps q xs a. Function ps (Eff xs q) -- ^ Handle the foremost action
  -> Eff (k >: Action ps q ': xs) a -> Eff xs a
peelAction0 wrap = go where
  go m = case debone m of
    Return a -> return a
    Instruction i t :>>= k -> testMembership i
      (\Refl -> case t of
        (_ :: Action ps q x) ->
          let run :: forall t. Function t (Eff xs q) -> Action t q x -> Eff xs a
              run f AResult = f >>= go . k
              run f (AArgument x a) = run (f x) a
          in run wrap t)
      $ \j -> rebindEff0 (Instruction j t) (go . k)
{-# INLINE peelAction0 #-}

-- | The reader monad is characterised by a type equality between the result
-- type and the enviroment type.
type ReaderEff = (:~:)

-- | Fetch the environment.
askEff :: forall k r xs. Lookup xs k (ReaderEff r)
  => Proxy k -> Eff xs r
askEff p = liftEff p Refl
{-# INLINE askEff #-}

-- | Pass the environment to a function.
asksEff :: forall k r xs a. Lookup xs k (ReaderEff r)
  => Proxy k -> (r -> a) -> Eff xs a
asksEff p = liftsEff p Refl
{-# INLINE asksEff #-}

-- | Modify the enviroment locally.
localEff :: forall k r xs a. Lookup xs k (ReaderEff r)
  => Proxy k -> (r -> r) -> Eff xs a -> Eff xs a
localEff _ f = go where
  go m = case debone m of
    Return a -> return a
    Instruction i t :>>= k -> case compareMembership
      (association :: Membership xs (k >: ReaderEff r)) i of
        Left _ -> boned $ Instruction i t :>>= go . k
        Right Refl -> case t of
          Refl -> boned $ Instruction i t :>>= go . k . f
{-# INLINE localEff #-}

-- | Run the frontal reader effect.
runReaderEff :: forall k r xs a. Eff (k >: ReaderEff r ': xs) a -> r -> Eff xs a
runReaderEff m r = peelEff0 return (\Refl k -> k r) m
{-# INLINE runReaderEff #-}

-- | Get the current state.
getEff :: forall k s xs. Lookup xs k (State s)
  => Proxy k -> Eff xs s
getEff k = liftEff k get
{-# INLINE getEff #-}

-- | Pass the current state to a function.
getsEff :: forall k s a xs. Lookup xs k (State s)
  => Proxy k -> (s -> a) -> Eff xs a
getsEff k = liftsEff k get
{-# INLINE getsEff #-}

-- | Replace the state with a new value.
putEff :: forall k s xs. Lookup xs k (State s)
  => Proxy k -> s -> Eff xs ()
putEff k = liftEff k . put
{-# INLINE putEff #-}

-- | Modify the state.
modifyEff :: forall k s xs. Lookup xs k (State s)
  => Proxy k -> (s -> s) -> Eff xs ()
modifyEff k f = liftEff k $ state $ \s -> ((), f s)
{-# INLINE modifyEff #-}

-- | Lift a state modification function.
stateEff :: forall k s xs a. Lookup xs k (State s)
  => Proxy k -> (s -> (a, s)) -> Eff xs a
stateEff k = liftEff k . state
{-# INLINE stateEff #-}

contState :: State s a -> (a -> s -> r) -> s -> r
contState m k s = let (a, s') = runState m s in k a $! s'

-- | Run the frontal state effect.
runStateEff :: forall k s xs a. Eff (k >: State s ': xs) a -> s -> Eff xs (a, s)
runStateEff = peelEff1 (\a s -> return (a, s)) contState
{-# INLINE runStateEff #-}

-- | Run the frontal state effect and return the final state.
execStateEff :: forall k s xs a. Eff (k >: State s ': xs) a -> s -> Eff xs s
execStateEff = peelEff1 (const return) contState
{-# INLINE execStateEff #-}

-- | Run the frontal state effect and return the final result.
evalStateEff :: forall k s xs a. Eff (k >: State s ': xs) a -> s -> Eff xs a
evalStateEff = peelEff1 (const . return) contState
{-# INLINE evalStateEff #-}

-- | @(,)@ already is a writer monad.
type WriterEff w = (,) w

-- | Write the second element and return the first element.
writerEff :: forall k w xs a. (Lookup xs k (WriterEff w))
  => Proxy k -> (a, w) -> Eff xs a
writerEff k (a, w) = liftEff k (w, a)
{-# INLINE writerEff #-}

-- | Write a value.
tellEff :: forall k w xs. (Lookup xs k (WriterEff w))
  => Proxy k -> w -> Eff xs ()
tellEff k w = liftEff k (w, ())
{-# INLINE tellEff #-}

-- | Squash the outputs into one step and return it.
listenEff :: forall k w xs a. (Lookup xs k (WriterEff w), Monoid w)
  => Proxy k -> Eff xs a -> Eff xs (a, w)
listenEff p = go mempty where
  go w m = case debone m of
    Return a -> writerEff p ((a, w), w)
    Instruction i t :>>= k -> case compareMembership (association :: Membership xs (k ':> (,) w)) i of
      Left _ -> boned $ Instruction i t :>>= go w . k
      Right Refl -> let (w', a) = t
                        !w'' = mappend w w' in go w'' (k a)
{-# INLINE listenEff #-}

-- | Modify the output using the function in the result.
passEff :: forall k w xs a. (Lookup xs k (WriterEff w), Monoid w)
  => Proxy k -> Eff xs (a, w -> w) -> Eff xs a
passEff p = go mempty where
  go w m = case debone m of
    Return (a, f) -> writerEff p (a, f w)
    Instruction i t :>>= k -> case compareMembership (association :: Membership xs (k ':> (,) w)) i of
      Left _ -> boned $ Instruction i t :>>= go w . k
      Right Refl -> let (w', a) = t
                        !w'' = mappend w w' in go w'' (k a)
{-# INLINE passEff #-}

contWriter :: Monoid w => (w, a) -> (a -> w -> r) -> w -> r
contWriter (w', a) k w = k a $! mappend w w'

-- | Run the frontal writer effect.
runWriterEff :: forall k w xs a. Monoid w => Eff (k >: WriterEff w ': xs) a -> Eff xs (a, w)
runWriterEff = peelEff1 (\a w -> return (a, w)) contWriter `flip` mempty
{-# INLINE runWriterEff #-}

-- | Run the frontal state effect.
execWriterEff :: forall k w xs a. Monoid w => Eff (k >: WriterEff w ': xs) a -> Eff xs w
execWriterEff = peelEff1 (const return) contWriter `flip` mempty
{-# INLINE execWriterEff #-}

-- | An effect with no result
type MaybeEff = Const ()

-- | Break out of the computation. Similar to 'Nothing'.
nothingEff :: Lookup xs k MaybeEff => Proxy k -> Eff xs a
nothingEff = flip throwEff ()

-- | Run an effect which may fail in the name of @k@.
runMaybeEff :: forall k xs a. Eff (k >: MaybeEff ': xs) a -> Eff xs (Maybe a)
runMaybeEff = peelEff0 (return . Just) $ \_ _ -> return Nothing
{-# INLINE runMaybeEff #-}

-- | Throwing an exception
type EitherEff = Const

-- | Throw an exception @e@, throwing the rest of the computation away.
throwEff :: Lookup xs k (EitherEff e) => Proxy k -> e -> Eff xs a
throwEff k = liftEff k . Const
{-# INLINE throwEff #-}

-- | Attach a handler for an exception.
catchEff :: forall k e xs a. (Lookup xs k (EitherEff e))
  => Proxy k -> Eff xs a -> (e -> Eff xs a) -> Eff xs a
catchEff _ m0 handler = go m0 where
  go m = case debone m of
    Return a -> return a
    Instruction i t :>>= k -> case compareMembership (association :: Membership xs (k ':> Const e)) i of
      Left _ -> boned $ Instruction i t :>>= go . k
      Right Refl -> handler (getConst t)
{-# INLINE catchEff #-}

-- | Run an action and abort on 'throwEff'.
runEitherEff :: forall k e xs a. Eff (k >: EitherEff e ': xs) a -> Eff xs (Either e a)
runEitherEff = peelEff0 (return . Right) $ \(Const e) _ -> return $ Left e
{-# INLINE runEitherEff #-}

-- | Put a milestone on a computation.
tickEff :: Lookup xs k Identity => Proxy k -> Eff xs ()
tickEff k = liftEff k $ Identity ()
{-# INLINE tickEff #-}

mapHeadEff :: (forall x. s x -> t x) -> Eff ((k >: s) ': xs) a -> Eff ((k' >: t) ': xs) a
mapHeadEff f = hoistSkeleton $ \(Instruction i t) -> testMembership i 
  (\Refl -> Instruction leadership $ f t) 
  (\j -> Instruction (nextMembership j) t)

-- | Take a function and applies it to an Either effect iff the effect takes the form Left _.
mapLeftEff :: (e -> e') -> Eff ((k >: EitherEff e) ': xs) a -> Eff ((k >: EitherEff e') ': xs) a
mapLeftEff f = mapHeadEff (first f)

-- | Run a computation until the first call of 'tickEff'.
runIterEff :: Eff (k >: Identity ': xs) a
  -> Eff xs (Either a (Eff (k >: Identity ': xs) a))
runIterEff m = case debone m of
  Return a -> return (Left a)
  Instruction i t :>>= k -> testMembership i
    (\Refl -> return $ Right $ k $ runIdentity t)
    $ \j -> boned $ Instruction j t :>>= runIterEff . k

-- | Place a continuation-passing action.
contEff :: Lookup xs k (ContT r m) => Proxy k
  -> ((a -> m r) -> m r) -> Eff xs a
contEff k = liftEff k . ContT

-- | Unwrap a continuation.
runContEff :: forall k r xs a. Eff (k >: ContT r (Eff xs) ': xs) a
  -> (a -> Eff xs r)
  -> Eff xs r
runContEff m cont = case debone m of
  Return a -> cont a
  Instruction i t :>>= k -> testMembership i
    (\Refl -> runContT t (flip runContEff cont . k))
    $ \j -> boned $ Instruction j t :>>= flip runContEff cont . k

-- | Call a function with the current continuation as its argument
callCCEff :: Proxy k -> ((a -> Eff ((k >: ContT r (Eff xs)) : xs) b) -> Eff ((k >: ContT r (Eff xs)) : xs) a) -> Eff ((k >: ContT r (Eff xs)) : xs) a
callCCEff k f = contHead k . ContT $ \c -> runContEff (f (\x -> contHead k . ContT $ \_ -> c x)) c
  where
    contHead :: Proxy k -> ContT r (Eff xs) a -> Eff ((k >: ContT r (Eff xs)) ': xs) a
    contHead _ c = boned $ Instruction leadership c :>>= return
