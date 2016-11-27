{-# LANGUAGE PolyKinds, KindSignatures, TypeOperators, DataKinds, Rank2Types, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Data.Extensible.Tangle where

import Control.Monad.Trans.RWS.Strict
import Control.Monad.Trans.Class
import Data.Extensible.Class
import Data.Extensible.Field
import Data.Extensible.Product
import Data.Extensible.Internal.Rig

newtype TangleT xs m a = TangleT
    { unTangleT :: RWST (RecordOf (TangleT xs m) xs) () (RecordOf Maybe xs) m a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans (TangleT xs) where
    lift = TangleT . lift

hitch :: forall k v m xs. (Monad m, Associate k v xs)
    => FieldName k -> TangleT xs m v
hitch _ = TangleT $ do
    mem <- get
    case getField $ hlookup k mem of
        Just a -> return a
        Nothing -> do
            tangles <- ask
            a <- unTangleT $ getField $ hlookup k tangles
            modify $ over (pieceAt k) $ const $ Field (Just a)
            return a
  where
    k :: Membership xs (k ':> v)
    k = association

runTangles :: Monad m
    => RecordOf (TangleT xs m) xs -- tangle matrix
    -> RecordOf Maybe xs
    -> m (Record xs)
runTangles tangles rec0 = fst <$> evalRWST m tangles rec0 where
    m = htraverse (\(Field a) -> Field . pure <$> unTangleT a) tangles
