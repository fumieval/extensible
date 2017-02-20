{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Effect.Default
-- Copyright   :  (c) Fumiaki Kinoshita 2017
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
  , WriterDef
  , runWriterDef
  , MaybeDef
  , runMaybeDef
  , EitherDef
  , runEitherDef
) where
import Control.Applicative
import Data.Extensible.Effect
import Data.Extensible.Internal
import Control.Monad.Except
import Control.Monad.Reader.Class
import Control.Monad.State.Strict
import Control.Monad.Writer.Class

instance Associate "IO" IO xs => MonadIO (Eff xs) where
  liftIO = liftEff (Proxy :: Proxy "IO")

pReader :: Proxy "Reader"
pReader = Proxy

instance Associate "Reader" ((:~:) r) xs => MonadReader r (Eff xs) where
  ask = askEff pReader
  local = localEff pReader
  reader = asksEff pReader

pState :: Proxy "State"
pState = Proxy

instance Associate "State" (State s) xs => MonadState s (Eff xs) where
  get = getEff pState
  put = putEff pState
  state = stateEff pState

pWriter :: Proxy "Writer"
pWriter = Proxy

instance (Monoid w, Associate "Writer" ((,) w) xs) => MonadWriter w (Eff xs) where
  writer = writerEff pWriter
  tell = tellEff pWriter
  listen = listenEff pWriter
  pass = passEff pWriter

pEither :: Proxy "Either"
pEither = Proxy

instance (Associate "Either" (Const e) xs) => MonadError e (Eff xs) where
  throwError = throwEff pEither
  catchError = catchEff pEither

instance (Monoid e, Associate "Either" (Const e) xs) => Alternative (Eff xs) where
  empty = throwError mempty
  p <|> q = catchError p (const q)

instance (Monoid e, Associate "Either" (Const e) xs) => MonadPlus (Eff xs) where
  mzero = empty
  mplus = (<|>)

type ReaderDef r = "Reader" >: ReaderEff r

runReaderDef :: Eff (ReaderDef r ': xs) a -> r -> Eff xs a
runReaderDef = runReaderEff
{-# INLINE runReaderDef #-}

type StateDef s = "State" >: State s

runStateDef :: Eff (StateDef s ': xs) a -> s -> Eff xs (a, s)
runStateDef = runStateEff
{-# INLINE runStateDef #-}

type WriterDef w = "Writer" >: WriterEff w

runWriterDef :: Monoid w => Eff (WriterDef w ': xs) a -> Eff xs (a, w)
runWriterDef = runWriterEff
{-# INLINE runWriterDef #-}

type MaybeDef = "Maybe" >: EitherEff ()

runMaybeDef :: Eff (MaybeDef ': xs) a -> Eff xs (Maybe a)
runMaybeDef = runMaybeEff
{-# INLINE runMaybeDef #-}

type EitherDef e = "Either" >: EitherEff e

runEitherDef :: Eff (EitherDef e ': xs) a -> Eff xs (Either e a)
runEitherDef = runEitherEff
{-# INLINE runEitherDef #-}
