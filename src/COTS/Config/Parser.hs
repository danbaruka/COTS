{-# LANGUAGE OverloadedStrings #-}

-- | Configuration file parser
module COTS.Config.Parser
  ( loadConfig
  , parseConfigFile
  , parseJSONConfig
  , parseYAMLConfig
  ) where

import Control.Exception (catch, SomeException)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeEither')
import System.FilePath (takeExtension)

import COTS.Types

-- | Load configuration from file
loadConfig :: FilePath -> IO Config
loadConfig filepath = do
  content <- BS.readFile filepath
  case parseConfigFile filepath content of
    Left err -> error $ "Failed to parse config file: " ++ err
    Right config -> return config

-- | Parse configuration file based on extension
parseConfigFile :: FilePath -> ByteString -> Either String Config
parseConfigFile filepath content = case takeExtension filepath of
  ".json" -> parseJSONConfig content
  ".yaml" -> parseYAMLConfig content
  ".yml" -> parseYAMLConfig content
  ext -> Left $ "Unsupported file extension: " ++ ext

-- | Parse JSON configuration
parseJSONConfig :: ByteString -> Either String Config
parseJSONConfig content = case eitherDecode content of
  Left err -> Left $ "JSON parsing error: " ++ err
  Right config -> Right config

-- | Parse YAML configuration
parseYAMLConfig :: ByteString -> Either String Config
parseYAMLConfig content = case decodeEither' (BSS.concat $ BS.toChunks content) of
  Left err -> Left $ "YAML parsing error: " ++ show err
  Right config -> Right config 