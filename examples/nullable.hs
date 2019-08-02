{-# LANGUAGE DataKinds, TypeOperators, OverloadedLabels, OverloadedStrings #-}
import Control.Lens
import qualified Data.Aeson as J
import Data.Extensible
import Data.Maybe (fromMaybe)

type ConfigRec = '[ "columns" >: Int, "language_extensions" >: [String] ]

defaultConfig :: Record ConfigRec
defaultConfig = #columns @= 80 <: #language_extensions @= [] <: nil

main :: IO ()
main = do
  config <- hzipWith fromNullable defaultConfig <$> readConfig "path/to/config.json"
  putStrLn $ "columns: " ++ (show $ config ^. #columns)
  putStrLn $ "language_extensions: " ++ (show $ config ^. #language_extensions)

-- dummy
readConfig :: FilePath -> IO (ConfigRec :& Nullable (Field Identity))
readConfig _path = pure $ fromMaybe vacancy (J.decode "{\"columns\": 100}")
