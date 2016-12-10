{-# LANGUAGE PolyKinds, KindSignatures, TypeOperators, DataKinds, Rank2Types, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Data.Extensible.Tangle where

import Control.Monad.Trans.RWS.Strict
import Control.Monad.Trans.Class
import Data.Extensible.Class
import Data.Extensible.Field
import Data.Extensible.Product
import Data.Extensible.Internal.Rig
import Data.Extensible.Nullable
import Data.Extensible.Wrapper

-- | @'TangleT' h xs m@ is the monad of computations that may depend on the fields in 'xs'.
newtype TangleT h xs m a = TangleT
    { unTangleT :: RWST
        (RecordOf (Comp (TangleT h xs m) h) xs)
        ()
        (RecordOf (Nullable h) xs)
        m a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans (TangleT h xs) where
    lift = TangleT . lift

-- | Get a value of the specified field.
hitch :: forall k v m h xs. (Monad m, Associate k v xs) => FieldName k -> TangleT h xs m (h v)
hitch _ = hitchAt (association :: Membership xs (k ':> v))

-- | Take a value from the tangles. The result is memoized.
hitchAt :: Monad m => Membership xs kv -> TangleT h xs m (h (AssocValue kv))
hitchAt k = TangleT $ do
    mem <- get
    case getNullable $ getField $ hlookup k mem of
        Just a -> return a
        Nothing -> do
            tangles <- ask
            a <- unTangleT $ getComp $ getField $ hlookup k tangles
            modify $ over (pieceAt k) $ const $ Field $ Nullable $ Just a
            return a

-- | Run tangles and collect the results as a 'Record'.
runTangles :: Monad m
    => RecordOf (Comp (TangleT h xs m) h) xs -- ^ tangle matrix
    -> RecordOf (Nullable h) xs -- ^ pre-calculated values
    -> m (RecordOf h xs)
runTangles tangles rec0 = fst <$> evalRWST
    (unTangleT $ htraverseWithIndex (\k _ -> Field <$> hitchAt k) rec0)
    tangles rec0
