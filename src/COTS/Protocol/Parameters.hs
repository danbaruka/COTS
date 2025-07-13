{-# LANGUAGE OverloadedStrings #-}

-- | Cardano protocol parameters
module COTS.Protocol.Parameters
  ( defaultProtocolParameters
  , loadProtocolParameters
  , getNetworkParameters
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import COTS.Types

-- | Default protocol parameters for mainnet
defaultProtocolParameters :: ProtocolParameters
defaultProtocolParameters = ProtocolParameters
  { minFeeA = 44
  , minFeeB = 155381
  , maxTxSize = 16384
  , maxValSize = 5000
  , keyDeposit = 2000000
  , poolDeposit = 500000000
  , coinsPerUtxoSize = 4310
  , maxCollateralInputs = 3
  , collateralPercentage = 150
  , maxExecutionUnitsPerTransaction = ExecutionUnits
      { memory = 14000000
      , steps = 10000000000
      }
  }

-- | Load protocol parameters for specific network
loadProtocolParameters :: Network -> ProtocolParameters
loadProtocolParameters network = case network of
  Mainnet -> defaultProtocolParameters
  Testnet -> testnetProtocolParameters
  Preview -> previewProtocolParameters
  Preprod -> preprodProtocolParameters

-- | Testnet protocol parameters
testnetProtocolParameters :: ProtocolParameters
testnetProtocolParameters = defaultProtocolParameters
  { minFeeA = 44
  , minFeeB = 155381
  }

-- | Preview protocol parameters
previewProtocolParameters :: ProtocolParameters
previewProtocolParameters = defaultProtocolParameters
  { minFeeA = 44
  , minFeeB = 155381
  }

-- | Preprod protocol parameters
preprodProtocolParameters :: ProtocolParameters
preprodProtocolParameters = defaultProtocolParameters
  { minFeeA = 44
  , minFeeB = 155381
  }

-- | Get network-specific parameters
getNetworkParameters :: Network -> Text
getNetworkParameters network = case network of
  Mainnet -> "mainnet"
  Testnet -> "testnet"
  Preview -> "preview"
  Preprod -> "preprod" 