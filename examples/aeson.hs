{-# LANGUAGE TypeOperators, DataKinds, FlexibleContexts, FlexibleInstances, UndecidableInstances, PolyKinds, TemplateHaskell #-}
import Data.Aeson (FromJSON(..), withObject)
import Data.Extensible (Record, Field(..), KeyValue, AssocKey, Forall, hgenerateFor)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy
import Data.String (fromString)
import qualified Data.HashMap.Strict as HM

keyProxy :: proxy kv -> Proxy (AssocKey kv)
keyProxy _ = Proxy

instance Forall (KeyValue KnownSymbol FromJSON) xs => FromJSON (Record xs) where
  parseJSON = withObject "Object" $ \v -> hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol FromJSON))
    $ \m -> let k = symbolVal (keyProxy m) in case HM.lookup (fromString k) v of
      Just a -> Field <$> return <$> parseJSON a
      Nothing -> fail $ "Missing key: " ++ k
