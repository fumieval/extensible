{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
import Data.Void

-- extensible-effects
import qualified Control.Eff as ExtEff
import qualified Control.Eff.Reader.Strict as ExtEff
import qualified Control.Eff.Writer.Strict as ExtEff
import qualified Control.Eff.State.Strict as ExtEff

-- effin
import qualified Control.Effect as Effin
import qualified Control.Effect.Reader as Effin
import qualified Control.Effect.Writer as Effin
import qualified Control.Effect.State as Effin

-- freer
import qualified Control.Monad.Freer as Freer
import qualified Control.Monad.Freer.Reader as Freer
import qualified Control.Monad.Freer.Writer as Freer
import qualified Control.Monad.Freer.State as Freer

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.RWS.Strict

import Data.Extensible.Effect
import Data.Extensible.Effect.Default

import Criterion.Main

testExtEff :: (ExtEff.Member (ExtEff.Reader Int) r
  , ExtEff.Member (ExtEff.State Int) r
  , ExtEff.Member (ExtEff.Writer (Sum Int)) r)
  => ExtEff.Eff r ()
testExtEff = replicateM_ 100 $ do
  r :: Int <- ExtEff.ask
  s <- ExtEff.get
  ExtEff.tell (Sum s)
  ExtEff.put $! s + r

runExtEff :: ExtEff.Eff
  ( ExtEff.Reader Int
  ExtEff.:> ExtEff.State Int
  ExtEff.:> ExtEff.Writer (Sum Int)
  ExtEff.:> Void) a -> (Sum Int, (Int, a))
runExtEff = ExtEff.run
  . ExtEff.runMonoidWriter
  . ExtEff.runState 0
  . flip ExtEff.runReader 1

testEffin :: (Effin.EffectReader Int l
  , Effin.EffectState Int l
  , Effin.EffectWriter (Sum Int) l)
  => Effin.Effect l ()
testEffin = replicateM_ 100 $ do
  r <- Effin.ask
  s <- Effin.get
  Effin.tell (Sum s)
  Effin.put $! s + r

runEffin = Effin.runEffect
  . Effin.runWriter
  . Effin.runState 0
  . Effin.runReader 1

testFreer :: (Freer.Member (Freer.Reader Int) r
  , Freer.Member (Freer.State Int) r
  , Freer.Member (Freer.Writer (Sum Int)) r)
  => Freer.Eff r ()
testFreer = replicateM_ 100 $ do
  r :: Int <- Freer.ask
  s <- Freer.get
  Freer.tell (Sum s)
  Freer.put $! s + r

runFreer :: Freer.Eff '[Freer.Reader Int, Freer.State Int, Freer.Writer (Sum Int)] a
  -> ((a, Int), Sum Int)
runFreer = Freer.run
  . Freer.runWriter
  . flip Freer.runState 0
  . flip Freer.runReader 1

testMTL :: (MonadReader Int m, MonadState Int m, MonadWriter (Sum Int) m)
  => m ()
testMTL = replicateM_ 100 $ do
  r <- ask
  s <- get
  tell (Sum s)
  put $! s + r

runMTL :: ReaderT Int (StateT Int (Writer (Sum Int))) a -> ((a, Int), Sum Int)
runMTL = runWriter
  . flip runStateT 0
  . flip runReaderT 1

runExtensible :: Eff '[ReaderDef Int, StateDef Int, WriterDef (Sum Int)] a
  -> ((a, Int), Sum Int)
runExtensible = leaveEff
  . runWriterDef
  . flip runStateDef 0
  . flip runReaderDef 1

main = defaultMain
  [ bgroup "rws"
    [ bench "extensible" $ nf runExtensible testMTL
    , bench "mtl" $ nf runMTL testMTL
    , bench "mtl-RWS" $ nf (\m -> runRWS m 0 1) testMTL
    , bench "exteff" $ nf runExtEff testExtEff
    , bench "effin" $ nf runEffin testEffin
    , bench "freer-effects" $ nf runFreer testFreer
    ]
  ]
