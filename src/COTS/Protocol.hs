-- | Protocol module for COTS
module COTS.Protocol
  ( defaultProtocolParameters
  , loadProtocolParameters
  , validateTransaction
  ) where

import COTS.Protocol.Parameters
import COTS.Protocol.Consensus
import COTS.Types 