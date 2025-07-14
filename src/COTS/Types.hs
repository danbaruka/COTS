{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Core types for the Cardano Offline Transaction Simulator
module COTS.Types
  ( -- * Configuration types
    Config (..),
    Network (..),
    ProtocolParameters (..),
    ExecutionUnits (..),
    Wallet (..),
    UTXO (..),
    Amount (..),
    Asset (..),

    -- * Transaction types
    Transaction (..),
    TransactionInput (..),
    TransactionOutput (..),
    TransactionId (..),
    TxIndex (..),

    -- * Simulation types
    SimulationResult (..),
    SimulationDetails (..),
    SimulationError (..),
    FeeCalculation (..),

    -- * Plutus types
    PlutusScript (..),
    Datum (..),
    Redeemer (..),
    Validator (..),

    -- * Export types
    ExportFormat (..),
    CardanoCLIExport (..),
    KoiosExport (..),

    -- * Utility types
    Lovelace (..),
    Address (..),
    ScriptHash (..),
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Word (Word64)
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key

-- | Network type for Cardano
data Network
  = Mainnet
  | Testnet
  | Preview
  | Preprod
  deriving (Eq, Show, Generic)

instance FromJSON Network

instance ToJSON Network

instance Read Network where
  readsPrec _ s =
    case map toLower s of
      "mainnet" -> [(Mainnet, "")]
      "testnet" -> [(Testnet, "")]
      "preview" -> [(Preview, "")]
      "preprod" -> [(Preprod, "")]
      _ -> []

-- | Execution units for Plutus scripts
data ExecutionUnits = ExecutionUnits
  { memory :: Word64,
    steps :: Word64
  }
  deriving (Eq, Show, Generic)

instance FromJSON ExecutionUnits

instance ToJSON ExecutionUnits

-- | Protocol parameters for Cardano
data ProtocolParameters = ProtocolParameters
  { minFeeA :: Word64,
    minFeeB :: Word64,
    maxTxSize :: Word64,
    maxValSize :: Word64,
    keyDeposit :: Word64,
    poolDeposit :: Word64,
    coinsPerUtxoSize :: Word64,
    maxCollateralInputs :: Word64,
    collateralPercentage :: Word64,
    maxExecutionUnitsPerTransaction :: ExecutionUnits
  }
  deriving (Eq, Show, Generic)

instance FromJSON ProtocolParameters

instance ToJSON ProtocolParameters

-- | Asset representation
newtype Asset = Asset {unAsset :: Text}
  deriving (Eq, Show, Generic, Ord)

instance FromJSON Asset

instance ToJSON Asset

instance FromJSONKey Asset

instance ToJSONKey Asset

-- | Amount with lovelace and optional assets
data Amount = Amount
  { lovelace :: Word64,
    assets :: Map Asset Word64
  }
  deriving (Eq, Show, Generic)

instance FromJSON Amount where
  parseJSON = Aeson.withObject "Amount" $ \v -> do
    lovelace <- v Aeson..: "lovelace"
    assetsVal <- v Aeson..: "assets"
    assets <- case assetsVal of
      Aeson.Object o -> pure $ Map.fromList [(Asset (T.pack (Key.toString k)), n) | (k, Aeson.Number n') <- KeyMap.toList o, let n = round n']
      Aeson.Array arr -> fmap Map.fromList $ mapM parseAsset (V.toList arr)
      _ -> fail "assets must be an object or array"
    return $ Amount lovelace assets
    where
      parseAsset = Aeson.withObject "Asset" $ \o -> do
        assetId <- o Aeson..: "assetId"
        quantity <- o Aeson..: "quantity"
        return (Asset assetId, quantity)

instance ToJSON Amount

-- | UTXO representation
data UTXO = UTXO
  { txHash :: TransactionId,
    txIx :: TxIndex,
    amount :: Amount
  }
  deriving (Eq, Show, Generic)

instance FromJSON UTXO

instance ToJSON UTXO

-- | Wallet configuration
data Wallet = Wallet
  { name :: Text,
    address :: Address,
    utxos :: [UTXO]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Wallet

instance ToJSON Wallet

-- | Main configuration
data Config = Config
  { network :: Network,
    protocolParameters :: ProtocolParameters,
    wallets :: [Wallet]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Config

instance ToJSON Config

-- | Transaction ID
newtype TransactionId = TransactionId {unTransactionId :: Text}
  deriving (Eq, Show, Generic)

instance FromJSON TransactionId

instance ToJSON TransactionId

-- | Transaction index
newtype TxIndex = TxIndex {unTxIndex :: Word64}
  deriving (Eq, Show, Generic)

instance FromJSON TxIndex

instance ToJSON TxIndex

-- | Transaction input
data TransactionInput = TransactionInput
  { inputTxId :: TransactionId,
    inputTxIx :: TxIndex
  }
  deriving (Eq, Show, Generic)

instance FromJSON TransactionInput

instance ToJSON TransactionInput

-- | Transaction output
data TransactionOutput = TransactionOutput
  { outputAddress :: Address,
    outputAmount :: Amount,
    outputDatum :: Maybe Datum
  }
  deriving (Eq, Show, Generic)

instance FromJSON TransactionOutput

instance ToJSON TransactionOutput

-- | Transaction representation
data Transaction = Transaction
  { txId :: TransactionId,
    txInputs :: [TransactionInput],
    txOutputs :: [TransactionOutput],
    txFee :: Lovelace,
    txValidRange :: Maybe (Word64, Word64), -- slot range
    txScripts :: [PlutusScript],
    txDatums :: [Datum],
    txRedeemers :: [Redeemer]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Transaction

instance ToJSON Transaction

-- | Lovelace amount
newtype Lovelace = Lovelace {unLovelace :: Word64}
  deriving (Eq, Show, Generic)

instance FromJSON Lovelace

instance ToJSON Lovelace

-- | Cardano address (simplified for simulation)
newtype Address = Address {unAddress :: Text}
  deriving (Eq, Show, Generic)

instance FromJSON Address

instance ToJSON Address

-- | Script hash (simplified for simulation)
newtype ScriptHash = ScriptHash {unScriptHash :: Text}
  deriving (Eq, Show, Generic)

instance FromJSON ScriptHash

instance ToJSON ScriptHash

-- | Plutus script
data PlutusScript = PlutusScript
  { scriptHash :: ScriptHash,
    scriptBytes :: Text, -- Base16 encoded
    scriptType :: Text -- "PlutusScriptV1" or "PlutusScriptV2"
  }
  deriving (Eq, Show, Generic)

instance FromJSON PlutusScript

instance ToJSON PlutusScript

-- | Datum for Plutus scripts
data Datum = Datum
  { datumHash :: Text,
    datumBytes :: Text -- Base16 encoded
  }
  deriving (Eq, Show, Generic)

instance FromJSON Datum

instance ToJSON Datum

-- | Redeemer for Plutus scripts
data Redeemer = Redeemer
  { redeemerBytes :: Text, -- Base16 encoded
    redeemerExecutionUnits :: ExecutionUnits
  }
  deriving (Eq, Show, Generic)

instance FromJSON Redeemer

instance ToJSON Redeemer

-- | Validator script
data Validator = Validator
  { validatorScript :: PlutusScript,
    validatorDatum :: Datum,
    validatorRedeemer :: Redeemer
  }
  deriving (Eq, Show, Generic)

instance FromJSON Validator

instance ToJSON Validator

-- | Simulation errors
data SimulationError
  = InsufficientFunds Text
  | InvalidUTXO TransactionId TxIndex
  | ScriptExecutionFailed Text
  | ValidationFailed Text
  | InvalidAddress Text
  | ConfigurationError Text
  deriving (Eq, Show, Generic)

instance FromJSON SimulationError

instance ToJSON SimulationError

-- | Fee calculation result
data FeeCalculation = FeeCalculation
  { baseFee :: Lovelace,
    sizeFee :: Lovelace,
    scriptFee :: Lovelace,
    totalFee :: Lovelace
  }
  deriving (Eq, Show, Generic)

instance FromJSON FeeCalculation

instance ToJSON FeeCalculation

-- | Detailed simulation information
data SimulationDetails = SimulationDetails
  { inputUTXOs :: [UTXO],
    outputUTXOs :: [UTXO],
    changeUTXO :: Maybe UTXO,
    totalInputAmount :: Word64,
    totalOutputAmount :: Word64,
    simChangeAmount :: Word64,
    feeAmount :: Word64
  }
  deriving (Eq, Show, Generic)

instance FromJSON SimulationDetails

instance ToJSON SimulationDetails

-- | Simulation result
data SimulationResult = SimulationResult
  { success :: Bool,
    transaction :: Maybe Transaction,
    feeCalculation :: FeeCalculation,
    errors :: [SimulationError],
    finalUTXOs :: Map Text [UTXO], -- wallet name -> UTXOs
    executionUnits :: Maybe ExecutionUnits,
    simulationDetails :: SimulationDetails
  }
  deriving (Eq, Show, Generic)

instance FromJSON SimulationResult

instance ToJSON SimulationResult

-- | Export formats
data ExportFormat
  = CardanoCLI
  | Koios
  | JSON
  deriving (Eq, Show, Generic)

instance FromJSON ExportFormat

instance ToJSON ExportFormat

instance Read ExportFormat where
  readsPrec _ s =
    case map toLower s of
      "cardanocli" -> [(CardanoCLI, "")]
      "koios" -> [(Koios, "")]
      "json" -> [(JSON, "")]
      _ -> []
    where
      toLower = Data.Char.toLower

-- | Cardano CLI export format
data CardanoCLIExport = CardanoCLIExport
  { command :: Text,
    arguments :: [Text],
    description :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON CardanoCLIExport

instance ToJSON CardanoCLIExport

-- | Koios API export format
data KoiosExport = KoiosExport
  { endpoint :: Text,
    method :: Text,
    headers :: Map Text Text,
    body :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON KoiosExport

instance ToJSON KoiosExport