{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, PolyKinds, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints#-}
import Data.Extensible
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Skeleton

decEffectSuite [d|
  data Example x where
    Reset :: Int -> Example ()
    PrintString :: String -> Example ()
    Hello :: Example ()
    Count :: Example Int
    |]

mkField "Reset PrintString Hello Count"

test :: IncludeAssoc xs Example => Eff xs ()
test = do
  hello
  hello
  n <- count
  printString (show n)
  reset 0
  n' <- count
  printString (show n')

-- | Object-like stateful handler
newtype Methods xs m = Methods
  { getMethods :: RecordOf (Interpreter (WriterT (Methods xs m) m)) xs }

runMethods :: Monad m => Methods xs m -> Eff xs a -> m (a, Methods xs m)
runMethods rec eff = case handleEff (getMethods rec) eff of
  Return a -> return (a, rec)
  m :>>= k -> do
    (a, rec') <- runWriterT m
    runMethods rec' (k a)

example :: Int -> Methods Example IO
example n = Methods
  $ _Reset @!? do \n' -> writer ((), example n')
  <: _PrintString @!? do \str -> WriterT $ ((), example n) <$ putStrLn str
  <: _Hello @!? do WriterT $ ((), example $ n + 1) <$ putStrLn "Hello!"
  <: _Count @!? do writer (n, example n)
  <: nil

takePrintString :: MonadIO (Eff xs) => Eff (PrintString ': xs) a -> Eff xs a
takePrintString = peelAction0 $ liftIO . putStrLn
