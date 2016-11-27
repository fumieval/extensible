{-# LANGUAGE PolyKinds, KindSignatures, TypeOperators, DataKinds, Rank2Types, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Data.Extensible.Tangle where

import Control.Monad.Trans.RWS.Strict
import Control.Monad.Trans.Class
import Data.Extensible.Class
import Data.Extensible.Field
import Data.Extensible.Product
import Data.Extensible.Internal.Rig

-- | @'TangleT' xs m@ is the monad of computations that may depend on the fields in 'xs'.
newtype TangleT xs m a = TangleT
    { unTangleT :: RWST (RecordOf (TangleT xs m) xs) () (RecordOf Maybe xs) m a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans (TangleT xs) where
    lift = TangleT . lift

-- | Get a value of the specified field.
hitch :: forall k v m xs. (Monad m, Associate k v xs) => FieldName k -> TangleT xs m v
hitch _ = hitchAt (association :: Membership xs (k ':> v))

-- | Take a value from the tangles. The result is memoized.
hitchAt :: Monad m => Membership xs kv -> TangleT xs m (AssocValue kv)
hitchAt k = TangleT $ do
    mem <- get
    case getField $ hlookup k mem of
        Just a -> return a
        Nothing -> do
            tangles <- ask
            a <- unTangleT $ getField $ hlookup k tangles
            modify $ over (pieceAt k) $ const $ Field (Just a)
            return a

-- | Run tangles and collect the results as a 'Record'.
runTangles :: Monad m
    => RecordOf (TangleT xs m) xs -- ^ tangle matrix
    -> RecordOf Maybe xs -- ^ pre-calculated values
    -> m (Record xs)
runTangles tangles rec0 = fst <$> evalRWST
    (unTangleT $ htraverseWithIndex (\k _ -> Field <$> pure <$> hitchAt k) rec0)
    tangles rec0
