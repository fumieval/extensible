{-# LANGUAGE TypeOperators, PolyKinds, FlexibleContexts, FlexibleInstances, TemplateHaskell, DataKinds #-}
import Control.Monad.Trans.Class
import Data.Extensible
import Data.Functor.Identity
import Data.Proxy

mkField "foo bar baz"

type Rec = ["foo" :> String, "bar" :> Int, "baz" :> Double, "qux" :> Bool]

class MakeRec kv where
  make :: proxy kv -> TangleT (Field Identity) Rec IO (AssocValue kv)

instance MakeRec ("foo" :> String) where
  make _ = lift getLine

instance MakeRec ("bar" :> Int) where
  make _ = lift $ length <$> getLine

instance MakeRec ("baz" :> Double) where
  make _ = lift readLn

instance MakeRec ("qux" :> Bool) where
  make _ = do
    str <- lasso foo
    x <- lasso baz
    return $ str == show x

makeRec :: IO (Record Rec)
makeRec = runTangles
  (htabulateFor (Proxy :: Proxy MakeRec)
    $ \m -> Comp $ Field . pure <$> make m)
  (wrench nil)
