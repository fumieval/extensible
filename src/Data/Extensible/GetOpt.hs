{-# LANGUAGE TypeFamilies, LambdaCase, DeriveFunctor, StandaloneDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.GetOpt
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- A wrapper for 'System.Console.GetOpt'
--------------------------------------------------------------------------------
module Data.Extensible.GetOpt (OptionDescr(..)
  , OptDescr'
  , optNoArg
  , optReqArg
  , optionNoArg
  , optionReqArg
  , getOptRecord
  , withGetOpt) where

import Control.Monad.IO.Class
import Data.Extensible.Class
import Data.Extensible.Field
import Data.Extensible.Internal.Rig
import Data.Extensible.Product
import Data.Extensible.Wrapper
import Data.Functor.Identity
import Data.List (foldl')
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

-- | 'OptDescr' with a default
data OptionDescr h a = forall s. OptionDescr (s -> h a) !s (OptDescr (s -> s))

deriving instance Functor h => Functor (OptionDescr h)

supplyOption :: Maybe String -> OptionDescr h a -> OptionDescr h a
supplyOption str od@(OptionDescr k s opt@(Option _ _ arg _)) = case (str, arg) of
  (Just a, ReqArg f _) -> OptionDescr k (f a s) opt
  (Nothing, NoArg f) -> OptionDescr k (f s) opt
  (a, OptArg f _) -> OptionDescr k (f a s) opt
  _ -> od

extendArg :: (Maybe String -> a -> b) -> ArgDescr a -> ArgDescr b
extendArg f (NoArg a) = NoArg $ f Nothing a
extendArg f (ReqArg a ph) = ReqArg (\s -> f (Just s) (a s)) ph
extendArg f (OptArg a ph) = OptArg (f <*> a) ph

type OptDescr' = OptionDescr Identity

instance Wrapper (OptionDescr h) where
  type Repr (OptionDescr h) a = OptionDescr h a
  _Wrapper = id

-- | Option without an argument; the result is the total count of this option.
optNoArg :: [Char] -- ^ short option
    -> [String] -- ^ long option
    -> String -- ^ explanation
    -> OptDescr' Int
optNoArg = optionNoArg Identity

optionNoArg :: (Int -> h a) -> [Char] -> [String] -> String -> OptionDescr h a
optionNoArg f ss ls expl = OptionDescr f 0 $ Option ss ls (NoArg (+1)) expl

-- | Option with an argument
optReqArg :: [Char] -- ^ short option
    -> [String] -- ^ long option
    -> String -- ^ placeholder
    -> String -- ^ explanation
    -> OptDescr' [String]
optReqArg = optionReqArg Identity

optionReqArg :: ([String] -> h a) -> [Char] -> [String] -> String -> String -> OptionDescr h a
optionReqArg f ss ls ph expl = OptionDescr f [] $ Option ss ls (ReqArg (:) ph) expl

getOptRecord :: RecordOf (OptionDescr h) xs -- ^ a record of option descriptors
    -> [String] -- ^ arguments
    -> (RecordOf h xs, [String], [String], String -> String) -- ^ (result, remaining non-options, errors, usageInfo)
getOptRecord descs args = (result, rs, es, flip usageInfo updaters) where
  (fs, rs, es) = getOpt Permute updaters args
  updaters = hfoldrWithIndex
      (\i (Field (OptionDescr _ _ (Option ss ls arg expl))) -> (:)
          $ Option ss ls (extendArg (\a _ -> over (pieceAt i) (liftField (supplyOption a))) arg) expl)
      [] descs
  result = hmap (\(Field (OptionDescr k x _)) -> Field (k x))
      $ foldl' (flip id) descs fs

-- | When there's an error, print it along with the usage info to stderr
-- and terminate with 'exitFailure'.
withGetOpt :: MonadIO m => RecordOf (OptionDescr h) xs
  -> (RecordOf h xs -> [String] -> m a) -> m a
withGetOpt descs k = getOptRecord descs <$> liftIO getArgs >>= \case
  (r, xs, [], _) -> k r xs
  (_, _, errs, usage) -> liftIO $ do
    mapM_ (hPutStrLn stderr) errs
    getProgName >>= die . usage
