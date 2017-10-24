{-# LANGUAGE OverloadedLabels, LambdaCase #-}
import Control.Lens
import Data.Extensible
import Data.Extensible.GetOpt

main :: IO ()
main = withGetOpt opts $ \r _args -> do
  putStrLn $ "verbose: " ++ show (r ^. #verbose > 0)
  putStrLn $ "extra: " ++ show (r ^? #extra. folded)
  where
    opts = #verbose @= optNoArg "v" ["verbose"] "verbose"
      <: #extra @= optReqArg "e" ["extra"] "ARG" "extra arguments"
      <: nil
