-- | Configuration module for COTS
module COTS.Config
  ( loadConfig
  , validateConfig
  , parseConfigFile
  ) where

import COTS.Config.Parser
import COTS.Config.Validation
import COTS.Types 