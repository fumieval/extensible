{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Aeson (FromJSON(..), withObject)
import Data.Extensible
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy
import Data.String (fromString)
import qualified Data.HashMap.Strict as HM

instance Forall (KeyValue KnownSymbol FromJSON) xs => FromJSON (Record xs) where
  parseJSON = withObject "Object" $ \v -> hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol FromJSON))
    $ \m -> let k = symbolVal (proxyAssocKey m) in case HM.lookup (fromString k) v of
      Just a -> Field <$> return <$> parseJSON a
      Nothing -> fail $ "Missing key: " ++ k
