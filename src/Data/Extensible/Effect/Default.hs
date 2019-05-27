{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Effect.Default
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Default monad runners and 'MonadIO', 'MonadReader', 'MonadWriter',
-- 'MonadState', 'MonadError' instances
-----------------------------------------------------------------------------
module Data.Extensible.Effect.Default (
  ReaderDef
  , runReaderDef
  , StateDef
  , runStateDef
  , evalStateDef
  , execStateDef
  , WriterDef
  , runWriterDef
  , execWriterDef
  , MaybeDef
  , runMaybeDef
  , EitherDef
  , runEitherDef
  , ContDef
  , runContDef
) where
import Control.Applicative
import Data.Extensible.Effect
import Control.Monad.Except
import Control.Monad.Catch
import Control.Monad.Cont
import Control.Monad.Reader.Class
import Control.Monad.Skeleton
import Control.Monad.State.Strict
#if MIN_VERSION_resourcet(1,2,0)
import Control.Monad.Trans.Resource
#endif
import Control.Monad.Writer.Class
import Data.Type.Equality
import Type.Membership

instance (MonadIO m, Lookup xs "IO" m) => MonadIO (Eff xs) where
  liftIO = liftEff (Proxy :: Proxy "IO") . liftIO

#if MIN_VERSION_resourcet(1,2,0)
instance (MonadResource m, Lookup xs "IO" m) => MonadResource (Eff xs) where
  liftResourceT = liftEff (Proxy :: Proxy "IO") . liftResourceT
#endif

instance (MonadThrow m, Lookup xs "IO" m) => MonadThrow (Eff xs) where
  throwM = liftEff (Proxy :: Proxy "IO") . throwM

instance (MonadCatch m, Lookup xs "IO" m) => MonadCatch (Eff xs) where
  catch m0 h = go m0 where
    go m = case debone m of
      Return a -> return a
      Instruction i t :>>= k -> case compareMembership (association :: Membership xs ("IO" ':> m)) i of
        Left _ -> boned $ Instruction i t :>>= go . k
        Right Refl -> boned $ Instruction i (try t) :>>= go . either h k

pReader :: Proxy "Reader"
pReader = Proxy

instance Lookup xs "Reader" ((:~:) r) => MonadReader r (Eff xs) where
  ask = askEff pReader
  local = localEff pReader
  reader = asksEff pReader

pState :: Proxy "State"
pState = Proxy

instance Lookup xs "State" (State s) => MonadState s (Eff xs) where
  get = getEff pState
  put = putEff pState
  state = stateEff pState

pWriter :: Proxy "Writer"
pWriter = Proxy

instance (Monoid w, Lookup xs "Writer" ((,) w)) => MonadWriter w (Eff xs) where
  writer = writerEff pWriter
  tell = tellEff pWriter
  listen = listenEff pWriter
  pass = passEff pWriter

pEither :: Proxy "Either"
pEither = Proxy

instance (Lookup xs "Either" (Const e)) => MonadError e (Eff xs) where
  throwError = throwEff pEither
  catchError = catchEff pEither

-- | A bit dubious
instance (Monoid e, Lookup xs "Either" (Const e)) => Alternative (Eff xs) where
  empty = throwError mempty
  p <|> q = catchError p (const q)

instance (Monoid e, Lookup xs "Either" (Const e)) => MonadPlus (Eff xs) where
  mzero = empty
  mplus = (<|>)

pCont :: Proxy "Cont"
pCont = Proxy

instance MonadCont (Eff ((ContDef r (Eff xs)) ': xs)) where
  callCC = callCCEff pCont

-- | mtl-compatible reader
type ReaderDef r = "Reader" >: ReaderEff r

-- | Specialised version of 'runReaderEff' compatible with the 'MonadReader' instance.
runReaderDef :: Eff (ReaderDef r ': xs) a -> r -> Eff xs a
runReaderDef = runReaderEff
{-# INLINE runReaderDef #-}

-- | mtl-compatible state
type StateDef s = "State" >: State s

-- | 'runStateEff' specialised for the 'MonadState' instance.
runStateDef :: Eff (StateDef s ': xs) a -> s -> Eff xs (a, s)
runStateDef = runStateEff
{-# INLINE runStateDef #-}

-- | 'evalStateEff' specialised for the 'MonadState' instance.
evalStateDef :: Eff (StateDef s ': xs) a -> s -> Eff xs a
evalStateDef = evalStateEff
{-# INLINE evalStateDef #-}

-- | 'execStateEff' specialised for the 'MonadState' instance.
execStateDef :: Eff (StateDef s ': xs) a -> s -> Eff xs s
execStateDef = execStateEff
{-# INLINE execStateDef #-}

-- | mtl-compatible writer
type WriterDef w = "Writer" >: WriterEff w

-- | 'runWriterDef' specialised for the 'MonadWriter' instance.
runWriterDef :: Monoid w => Eff (WriterDef w ': xs) a -> Eff xs (a, w)
runWriterDef = runWriterEff
{-# INLINE runWriterDef #-}

-- | 'execWriterDef' specialised for the 'MonadWriter' instance.
execWriterDef :: Monoid w => Eff (WriterDef w ': xs) a -> Eff xs w
execWriterDef = execWriterEff
{-# INLINE execWriterDef #-}

-- | Same as @'EitherDef' ()@
type MaybeDef = "Either" >: EitherEff ()

-- | Similar to 'runMaybeT', but on 'Eff'
runMaybeDef :: Eff (MaybeDef ': xs) a -> Eff xs (Maybe a)
runMaybeDef = runMaybeEff
{-# INLINE runMaybeDef #-}

-- | mtl-compatible either effect
type EitherDef e = "Either" >: EitherEff e

-- | Similar to 'runExceptT', but on 'Eff'
runEitherDef :: Eff (EitherDef e ': xs) a -> Eff xs (Either e a)
runEitherDef = runEitherEff
{-# INLINE runEitherDef #-}

-- | mtl-compatible continuation
type ContDef r m = "Cont" >: ContT r m

-- | 'runContEff' specialised for the 'MonadCont' instance.
runContDef :: Eff (ContDef r (Eff xs) ': xs) a -> (a -> Eff xs r) -> Eff xs r
runContDef = runContEff
{-# INLINE runContDef #-}
