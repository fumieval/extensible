module Data.Extensible.Util
  ( fromHashMapOf
  , strings )
  where

import Prelude hiding (fail)
import Control.Arrow
import Control.Monad.Fail
import Data.Extensible.Product
import Data.Extensible.Field
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Hashable (Hashable)
import Data.Proxy
import Data.String
import qualified Data.HashMap.Strict as HM
import GHC.TypeLits

fromHashMapOf :: (Eq k, Hashable k, IsString k, Forall (KeyIs KnownSymbol) xs, MonadFail m)
  => RecordOf (Compose (Kleisli m v) h) xs
  -> HM.HashMap k v
  -> m (RecordOf h xs)
fromHashMapOf r m = hgenerateFor (Proxy :: Proxy (KeyIs KnownSymbol))
  (\k -> case HM.lookup (stringAssocKey k) m of
    Nothing -> fail $ "fromMapWith: key not found: " ++ stringAssocKey k
    Just v -> fmap Field $ runKleisli (getCompose $ getField $ hlookup k r) v)

strings :: (Applicative m, Forall (ValueIs IsString) xs)
  => RecordOf (Compose (Kleisli m String) Identity) xs
strings = hrepeatFor (Proxy :: Proxy (ValueIs IsString))
  $ Field $ Compose $ Kleisli $ pure . pure . fromString
