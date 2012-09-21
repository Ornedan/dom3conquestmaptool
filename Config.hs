{-# LANGUAGE OverloadedStrings #-}
module Config where

import qualified Data.ByteString as BS

import Control.Applicative
import Data.Map
import Data.Yaml

import Types


--------------------------------------------------------------------------------

instance FromJSON Colour where
  parseJSON (Object v) = Colour   <$>
                         v .: "r" <*>
                         v .: "g" <*>
                         v .: "b"

instance FromJSON Config where
  parseJSON (Object v) = Config              <$>
                         v .: "borderColour" <*>
                         v .: "wraparound"   <*>
                         v .: "nations"      <*>
                         (mapKeys read <$> v .: "provinces")

loadConfig :: FilePath -> IO Config
loadConfig path = do
  yaml <- BS.readFile path
  case decodeEither yaml of
    Left err     -> error $ "Failed to load config file: " ++ err
    Right config -> return config
