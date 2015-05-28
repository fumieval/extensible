{-# LANGUAGE TypeOperators, DataKinds, TemplateHaskell, FlexibleContexts #-}
import Data.Extensible
import Control.Lens
import Control.Monad.State

mkField "foo bar baz"

statefulStuff :: State (Record '["foo" :> Int, "bar" :> Int, "baz" :> Float]) ()
statefulStuff = do
    v <- use foo
    bar += v
    baz .= 42

main = print $ execState statefulStuff
  $ foo @= 10 <: bar @= 0 <: baz @= 0 <: Nil
