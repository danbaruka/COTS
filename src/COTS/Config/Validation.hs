{-# LANGUAGE OverloadedStrings #-}

-- | Configuration validation
module COTS.Config.Validation
  ( validateConfig
  , ValidationError(..)
  , validateWallets
  , validateProtocolParameters
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import qualified Data.Map.Strict as Map

import COTS.Types

-- | Configuration validation errors
data ValidationError
  = InvalidNetwork Text
  | InvalidProtocolParameters Text
  | InvalidWallet Text
  | DuplicateWallet Text
  | InsufficientWallets
  deriving (Eq, Show)

-- | Validate configuration
validateConfig :: Config -> Either [ValidationError] ()
validateConfig config = do
  let errors = concat
        [ validateNetwork (network config)
        , validateProtocolParameters (protocolParameters config)
        , validateWallets (wallets config)
        ]
  
  case errors of
    [] -> Right ()
    errs -> Left errs

-- | Validate network
validateNetwork :: Network -> [ValidationError]
validateNetwork network = case network of
  Mainnet -> []
  Testnet -> []
  Preview -> []
  Preprod -> []
  _ -> [InvalidNetwork "Unknown network"]

-- | Validate protocol parameters
validateProtocolParameters :: ProtocolParameters -> [ValidationError]
validateProtocolParameters params = concat
  [ if minFeeA params == 0 then [InvalidProtocolParameters "minFeeA cannot be zero"] else []
  , if minFeeB params == 0 then [InvalidProtocolParameters "minFeeB cannot be zero"] else []
  , if maxTxSize params == 0 then [InvalidProtocolParameters "maxTxSize cannot be zero"] else []
  , if maxValSize params == 0 then [InvalidProtocolParameters "maxValSize cannot be zero"] else []
  , if keyDeposit params == 0 then [InvalidProtocolParameters "keyDeposit cannot be zero"] else []
  , if poolDeposit params == 0 then [InvalidProtocolParameters "poolDeposit cannot be zero"] else []
  , if coinsPerUtxoSize params == 0 then [InvalidProtocolParameters "coinsPerUtxoSize cannot be zero"] else []
  , if maxCollateralInputs params == 0 then [InvalidProtocolParameters "maxCollateralInputs cannot be zero"] else []
  , if collateralPercentage params == 0 then [InvalidProtocolParameters "collateralPercentage cannot be zero"] else []
  , validateExecutionUnits (maxExecutionUnitsPerTransaction params)
  ]

-- | Validate execution units
validateExecutionUnits :: ExecutionUnits -> [ValidationError]
validateExecutionUnits units = concat
  [ if memory units == 0 then [InvalidProtocolParameters "Execution units memory cannot be zero"] else []
  , if steps units == 0 then [InvalidProtocolParameters "Execution units steps cannot be zero"] else []
  ]

-- | Validate wallets
validateWallets :: [Wallet] -> [ValidationError]
validateWallets wallets = concat
  [ if null wallets then [InsufficientWallets] else []
  , concatMap validateWallet wallets
  , validateUniqueWallets wallets
  ]

-- | Validate individual wallet
validateWallet :: Wallet -> [ValidationError]
validateWallet wallet = concat
  [ if T.null (name wallet) then [InvalidWallet "Wallet name cannot be empty"] else []
  , if T.null (unAddress (address wallet)) then [InvalidWallet "Wallet address cannot be empty"] else []
  , concatMap (validateUTXO (name wallet)) (utxos wallet)
  ]

-- | Validate UTXO
validateUTXO :: Text -> UTXO -> [ValidationError]
validateUTXO walletName utxo = concat
  [ if T.null (unTransactionId (txHash utxo)) then [InvalidProtocolParameters $ "Invalid transaction hash for wallet " <> walletName] else []
  , if lovelace (amount utxo) == 0 && Map.null (assets (amount utxo)) then [InvalidProtocolParameters $ "UTXO amount cannot be zero for wallet " <> walletName] else []
  ]

-- | Validate unique wallet names
validateUniqueWallets :: [Wallet] -> [ValidationError]
validateUniqueWallets wallets = 
  let names = map name wallets
      duplicates = findDuplicates names
  in map DuplicateWallet duplicates

-- | Find duplicate elements in a list
findDuplicates :: Eq a => [a] -> [a]
findDuplicates xs = [x | x <- xs, length (filter (== x) xs) > 1] 