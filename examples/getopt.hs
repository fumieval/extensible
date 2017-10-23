{-# LANGUAGE OverloadedLabels #-}
import Control.Lens
import Data.Extensible
import Data.Extensible.GetOpt
import System.Environment

main = getArgs >>= \args -> case getOptRecord opts args of
  (_, _, _:_, usage) -> putStrLn $ usage "getopt.hs"
  (r, _, [], _) -> do
    putStrLn $ "verbose: " ++ show (r ^. #verbose)
    putStrLn $ "extra: " ++ show (r ^. #extra)
  where
    opts = #verbose @= optNoArg "v" ["verbose"] "verbose"
      <: #extra @= optReqArg "e" ["extra"] "ARG" "extra arguments"
      <: nil
