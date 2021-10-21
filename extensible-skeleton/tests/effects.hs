{-# LANGUAGE GADTs, DataKinds, FlexibleContexts, TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Main where
import Control.Monad.IO.Class
import Data.Extensible.Effect
import Data.Extensible.Effect.TH
import Data.Extensible.Effect.Default

decEffects [d|
  data Example a b x where
    Concrete :: Int -> Example a b ()
    PolyArg :: a -> Example a b ()
    PolyRes :: Example a b b
    PolyArgRes :: a -> Example a b b
    UnboundArg :: x -> Example a b ()
    UnboundRes :: Example a b x
--    ExtArg :: Show s => s -> Example a b ()
--    ExtRes :: Read s => Example a b s
  |]

decEffects [d|
  data Simple x where
    Simple :: Simple ()
  |]

handleSimple :: MonadIO (Eff xs) => Eff (Simple ': xs) a -> Eff xs a
handleSimple = peelAction0 (liftIO (putStrLn "hello"))

handleConcrete :: MonadIO (Eff xs) => Eff (Concrete ': xs) a -> Eff xs a
handleConcrete = peelAction0 (\i -> liftIO (print i))

handlePolyArg :: (Show a, MonadIO (Eff xs)) => Eff (PolyArg a ': xs) a -> Eff xs a
handlePolyArg = peelAction0 (\i -> liftIO (print i))

handlePolyRes :: (Monoid a, MonadIO (Eff xs)) => Eff (PolyRes a ': xs) a -> Eff xs a
handlePolyRes = peelAction0 (pure mempty)

handleUnboundArg :: (Monoid a, MonadIO (Eff xs)) => Eff (UnboundArg a ': xs) a -> Eff xs a
handleUnboundArg = peelAction0 (\_ -> pure ())

handleUnboundRes :: (Monoid a, MonadIO (Eff xs)) => Eff (UnboundRes a ': xs) a -> Eff xs a
handleUnboundRes = peelAction0 (pure mempty)

ex_simple :: IO ()
ex_simple = runIODef $ handleSimple simple

main :: IO ()
main = return ()
