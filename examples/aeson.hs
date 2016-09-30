{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Aeson hiding (KeyValue)
import Data.Constraint
import Data.Extensible
import Data.Monoid
import Data.Proxy
import Data.String (fromString)
import GHC.TypeLits (KnownSymbol, symbolVal)
import qualified Data.HashMap.Strict as HM

instance Forall (KeyValue KnownSymbol FromJSON) xs => FromJSON (Record xs) where
  parseJSON = withObject "Object" $ \v -> hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol FromJSON))
    $ \m -> let k = symbolVal (proxyAssocKey m) in case HM.lookup (fromString k) v of
      Just a -> Field <$> return <$> parseJSON a
      Nothing -> fail $ "Missing key: " ++ k

instance Forall (KeyValue KnownSymbol ToJSON) xs => ToJSON (Record xs) where
  toJSON rec = Object $ HM.fromList $ flip appEndo [] $ hfoldMap getConst'
    $ hzipWith (\(Comp Dict) v -> Const' $ Endo
      ((fromString $ symbolVal $ proxyAssocKey v, toJSON $ getField v):))
    (library :: Comp Dict (KeyValue KnownSymbol ToJSON) :* xs) rec
