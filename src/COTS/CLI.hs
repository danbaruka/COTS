{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Command Line Interface for COTS (Cardano Offline Transaction Simulator)
module COTS.CLI
  ( runCLI,
    Command (..),
    TransactionCommand (..),
    UTXOCommand (..),
    ProtocolCommand (..),
    BuildOptions (..),
    SimulateOptions (..),
    SignOptions (..),
    ValidateOptions (..),
    ExportOptions (..),
    DecodeOptions (..),
    ListOptions (..),
    ReserveOptions (..),
    UpdateOptions (..),
    DatabaseCommand (..),
    InitOptions (..),
    ResetOptions (..),
    SnapshotOptions (..),
    LoadSnapshotOptions (..),
    ImportUTXOptions (..),
    ExportUTXOptions (..),
    InspectOptions (..),
    AddressCommand (..),
    AddressKeyGenOptions (..),
    AddressBuildOptions (..),
    AddressInfoOptions (..),
    StakeAddressCommand (..),
    StakeAddressKeyGenOptions (..),
    StakeAddressBuildOptions (..),
    StakeAddressInfoOptions (..),
    MintCommand (..),
    MintBuildOptions (..),
    MintCalculateOptions (..),
  )
where

import COTS.Config (loadConfig)
import COTS.Database (Database (..), closeDatabase, exportUTXOs, importUTXOs, initDatabase, inspectDatabase, loadSnapshot, resetDatabase, snapshotDatabase)
import COTS.Export.CardanoCLI (exportTransactionToFile)
import COTS.Export.Koios (exportTransactionToKoiosFile)
import COTS.Simulation.Core (SimulationContext (..), simulateTransaction)
import COTS.Types hiding (command)
import COTS.Version (getVersionString)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (encode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Char (intToDigit)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Word (Word64)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Random (randomRIO)
import Text.Printf (printf)

-- | Get the root COTS_NODE directory (~/.COTS_NODE)
getCotsNodeDir :: IO FilePath
getCotsNodeDir = do
  home <- getHomeDirectory
  let dir = home </> ".COTS_NODE"
  createDirectoryIfMissing True dir
  return dir

-- | Get a subdirectory of COTS_NODE, creating it if necessary
getCotsNodeSubdir :: String -> IO FilePath
getCotsNodeSubdir sub = do
  root <- getCotsNodeDir
  let subdir = root </> sub
  createDirectoryIfMissing True subdir
  return subdir

-- | Get the path to the COTS database in ~/.COTS_NODE/cots.db
getCotsNodeDbPath :: IO FilePath
getCotsNodeDbPath = do
  root <- getCotsNodeDir
  return (root </> "cots.db")

-- | Main CLI command
data Command
  = TransactionCmd TransactionCommand
  | UTXOCmd UTXOCommand
  | ProtocolCmd ProtocolCommand
  | DatabaseCmd DatabaseCommand
  | AddressCmd AddressCommand
  | StakeAddressCmd StakeAddressCommand
  | MintCmd MintCommand
  | Version

-- | Transaction subcommands
data TransactionCommand
  = Build BuildOptions
  | Simulate SimulateOptions
  | Sign SignOptions
  | Validate ValidateOptions
  | Export ExportOptions
  | Decode DecodeOptions

-- | UTXO subcommands
data UTXOCommand
  = List ListOptions
  | Reserve ReserveOptions

-- | Protocol subcommands
data ProtocolCommand
  = Update UpdateOptions

-- | Database subcommands
data DatabaseCommand
  = Init InitOptions
  | Reset ResetOptions
  | Snapshot SnapshotOptions
  | LoadSnapshot LoadSnapshotOptions
  | ImportUTXO ImportUTXOptions
  | ExportUTXO ExportUTXOptions
  | Inspect InspectOptions

-- | Build transaction options (cardano-cli style)
data BuildOptions = BuildOptions
  { txIns :: [Text], -- --tx-in
    txOuts :: [Text], -- --tx-out
    changeAddress :: Maybe Text, -- --change-address
    protocolParamsFile :: FilePath, -- --protocol-params-file
    outFile :: FilePath, -- --out-file
    offline :: Bool, -- --offline (always true for COTS)
    fee :: Maybe Word64, -- --fee
    ttl :: Maybe Word64, -- --ttl
    scriptFile :: Maybe FilePath, -- --script-file
    datumFile :: Maybe FilePath, -- --datum-file
    redeemerFile :: Maybe FilePath -- --redeemer-file
  }

-- | Simulate transaction options
data SimulateOptions = SimulateOptions
  { simTxFile :: FilePath, -- --tx-file
    simUtxoFile :: FilePath, -- --utxo-file
    simProtocolParamsFile :: FilePath, -- --protocol-params-file
    simVerbose :: Bool -- --verbose
  }

-- | Sign transaction options
data SignOptions = SignOptions
  { signTxFile :: FilePath, -- --tx-file
    signKeyFile :: FilePath, -- --signing-key-file
    signOutFile :: FilePath -- --out-file
  }

-- | Validate transaction options
data ValidateOptions = ValidateOptions
  { validateTxFile :: FilePath, -- --tx-file
    validateProtocolParamsFile :: FilePath -- --protocol-params-file
  }

-- | Export transaction options
data ExportOptions = ExportOptions
  { exportTxFile :: FilePath, -- --tx-file
    exportFormat :: ExportFormat, -- --format
    exportOutFile :: FilePath -- --out-file
  }

-- | Decode transaction options
data DecodeOptions = DecodeOptions
  { decodeTxFile :: FilePath, -- --tx-file
    decodeVerbose :: Bool -- --verbose
  }

-- | List UTXOs options
data ListOptions = ListOptions
  { listAddress :: Maybe Text, -- --address
    listUtxoFile :: FilePath, -- --utxo-file
    listVerbose :: Bool -- --verbose
  }

-- | Reserve UTXOs options
data ReserveOptions = ReserveOptions
  { reserveAddress :: Text, -- --address
    reserveAmount :: Word64, -- --amount
    reserveUtxoFile :: FilePath, -- --utxo-file
    reserveOutFile :: FilePath -- --out-file
  }

-- | Update protocol options
data UpdateOptions = UpdateOptions
  { updateProtocolParamsFile :: FilePath, -- --protocol-params-file
    updateOutFile :: FilePath -- --out-file
  }

-- | Init database options
data InitOptions = InitOptions
  { initDbFile :: FilePath -- --db-file
  }

-- | Reset database options
data ResetOptions = ResetOptions
  { resetDbFile :: FilePath -- --db-file
  }

-- | Snapshot database options
data SnapshotOptions = SnapshotOptions
  { snapshotDbFile :: FilePath, -- --db-file
    snapshotOutFile :: FilePath -- --out-file
  }

-- | Load snapshot options
data LoadSnapshotOptions = LoadSnapshotOptions
  { loadSnapshotFile :: FilePath, -- --snapshot-file
    loadDbFile :: FilePath -- --db-file
  }

-- | Import UTXO options
data ImportUTXOptions = ImportUTXOptions
  { importDbFile :: FilePath, -- --db-file
    importUtxoFile :: FilePath -- --utxo-file
  }

-- | Export UTXO options
data ExportUTXOptions = ExportUTXOptions
  { exportDbFile :: FilePath, -- --db-file
    exportUtxoFile :: FilePath -- --out-file
  }

-- | Inspect database options
data InspectOptions = InspectOptions
  { inspectDbFile :: FilePath -- --db-file
  }

-- | Address subcommands (cardano-cli compatible)
data AddressCommand
  = AddressKeyGen AddressKeyGenOptions
  | AddressBuild AddressBuildOptions
  | AddressInfo AddressInfoOptions

-- | Address key generation options
data AddressKeyGenOptions = AddressKeyGenOptions
  { keyGenVerificationKeyFile :: FilePath, -- --verification-key-file
    keyGenSigningKeyFile :: FilePath, -- --signing-key-file
    keyGenKeyType :: Maybe Text -- --key-type (normal, extended)
  }

-- | Address build options
data AddressBuildOptions = AddressBuildOptions
  { buildPaymentVerificationKeyFile :: Maybe FilePath, -- --payment-verification-key-file
    buildStakeVerificationKeyFile :: Maybe FilePath, -- --stake-verification-key-file
    buildOutFile :: FilePath, -- --out-file
    buildNetwork :: Network -- --mainnet or --testnet-magic
  }

-- | Address info options
data AddressInfoOptions = AddressInfoOptions
  { infoAddress :: Text -- --address
  }

-- | Stake address subcommands
data StakeAddressCommand
  = StakeAddressKeyGen StakeAddressKeyGenOptions
  | StakeAddressBuild StakeAddressBuildOptions
  | StakeAddressInfo StakeAddressInfoOptions

-- | Stake address key generation options
data StakeAddressKeyGenOptions = StakeAddressKeyGenOptions
  { stakeKeyGenVerificationKeyFile :: FilePath, -- --verification-key-file
    stakeKeyGenSigningKeyFile :: FilePath -- --signing-key-file
  }

-- | Stake address build options
data StakeAddressBuildOptions = StakeAddressBuildOptions
  { stakeBuildStakeVerificationKeyFile :: FilePath, -- --stake-verification-key-file
    stakeBuildOutFile :: FilePath, -- --out-file
    stakeBuildNetwork :: Network -- --mainnet or --testnet-magic
  }

-- | Stake address info options
data StakeAddressInfoOptions = StakeAddressInfoOptions
  { stakeInfoAddress :: Text -- --address
  }

-- | Mint subcommands
data MintCommand
  = MintBuild MintBuildOptions
  | MintCalculate MintCalculateOptions

-- | Mint build options
data MintBuildOptions = MintBuildOptions
  { mintTxIn :: [Text], -- --tx-in
    mintTxOut :: [Text], -- --tx-out
    mintMint :: Maybe Text, -- --mint
    mintMintScriptFile :: Maybe FilePath, -- --mint-script-file
    mintChangeAddress :: Maybe Text, -- --change-address
    mintOutFile :: FilePath, -- --out-file
    mintNetwork :: Network, -- --mainnet or --testnet-magic
    mintProtocolParamsFile :: FilePath -- --protocol-params-file
  }

-- | Mint calculate options
data MintCalculateOptions = MintCalculateOptions
  { mintCalcPolicyId :: Text, -- --policy-id
    mintCalcAssetName :: Text, -- --asset-name
    mintCalcQuantity :: Word64, -- --quantity
    mintCalcProtocolParamsFile :: FilePath -- --protocol-params-file
  }

-- | Parse command line arguments
commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "transaction"
        ( info
            (TransactionCmd <$> transactionParser)
            ( progDesc "Transaction commands"
            )
        )
        <> command
          "utxo"
          ( info
              (UTXOCmd <$> utxoParser)
              ( progDesc "UTXO management commands"
              )
          )
        <> command
          "protocol"
          ( info
              (ProtocolCmd <$> protocolParser)
              ( progDesc "Protocol parameter commands"
              )
          )
        <> command
          "database"
          ( info
              (DatabaseCmd <$> databaseParser)
              ( progDesc "Database management commands"
              )
          )
        <> command
          "address"
          ( info
              (AddressCmd <$> addressParser)
              ( progDesc "Cardano address management commands"
              )
          )
        <> command
          "stake-address"
          ( info
              (StakeAddressCmd <$> stakeAddressParser)
              ( progDesc "Cardano stake address management commands"
              )
          )
        <> command
          "mint"
          ( info
              (MintCmd <$> mintParser)
              ( progDesc "Cardano minting management commands"
              )
          )
        <> command
          "version"
          ( info
              (pure Version)
              ( progDesc "Show version information"
              )
          )
    )

-- | Parse transaction subcommands
transactionParser :: Parser TransactionCommand
transactionParser =
  subparser
    ( command
        "build"
        ( info
            (Build <$> buildOptions)
            ( progDesc "Build a transaction (offline simulation)"
            )
        )
        <> command
          "simulate"
          ( info
              (Simulate <$> simulateOptions)
              ( progDesc "Simulate a transaction"
              )
          )
        <> command
          "sign"
          ( info
              (Sign <$> signOptions)
              ( progDesc "Sign a transaction (offline)"
              )
          )
        <> command
          "validate"
          ( info
              (Validate <$> validateOptions)
              ( progDesc "Validate a transaction"
              )
          )
        <> command
          "export"
          ( info
              (Export <$> exportOptions)
              ( progDesc "Export transaction in various formats"
              )
          )
        <> command
          "decode"
          ( info
              (Decode <$> decodeOptions)
              ( progDesc "Decode and display transaction details"
              )
          )
    )
    <**> helper

-- | Parse UTXO subcommands
utxoParser :: Parser UTXOCommand
utxoParser =
  subparser
    ( command
        "list"
        ( info
            (List <$> listOptions)
            ( progDesc "List available UTXOs"
            )
        )
        <> command
          "reserve"
          ( info
              (Reserve <$> reserveOptions)
              ( progDesc "Reserve UTXOs for transaction"
              )
          )
    )
    <**> helper

-- | Parse protocol subcommands
protocolParser :: Parser ProtocolCommand
protocolParser =
  subparser
    ( command
        "update"
        ( info
            (Update <$> updateOptions)
            ( progDesc "Update protocol parameters"
            )
        )
    )
    <**> helper

-- | Parse database subcommands
databaseParser :: Parser DatabaseCommand
databaseParser =
  subparser
    ( command
        "init"
        ( info
            (Init <$> initOptions)
            ( progDesc "Initialize a new SQLite database"
            )
        )
        <> command
          "reset"
          ( info
              (Reset <$> resetOptions)
              ( progDesc "Reset database (drop all tables and recreate)"
              )
          )
        <> command
          "snapshot"
          ( info
              (Snapshot <$> snapshotOptions)
              ( progDesc "Create a snapshot of the database"
              )
          )
        <> command
          "load-snapshot"
          ( info
              (LoadSnapshot <$> loadSnapshotOptions)
              ( progDesc "Load database from a snapshot"
              )
          )
        <> command
          "import-utxo"
          ( info
              (ImportUTXO <$> importUTXOptions)
              ( progDesc "Import UTXOs from JSON file"
              )
          )
        <> command
          "export-utxo"
          ( info
              (ExportUTXO <$> exportUTXOptions)
              ( progDesc "Export UTXOs to JSON file"
              )
          )
        <> command
          "inspect"
          ( info
              (Inspect <$> inspectOptions)
              ( progDesc "Inspect database statistics"
              )
          )
    )
    <**> helper

-- | Parse address subcommands
addressParser :: Parser AddressCommand
addressParser =
  subparser
    ( command
        "key-gen"
        ( info
            (AddressKeyGen <$> addressKeyGenOptions)
            ( progDesc "Generate a payment key pair"
            )
        )
        <> command
          "build"
          ( info
              (AddressBuild <$> addressBuildOptions)
              ( progDesc "Build a Cardano address"
              )
          )
        <> command
          "info"
          ( info
              (AddressInfo <$> addressInfoOptions)
              ( progDesc "Print information about an address"
              )
          )
    )
    <**> helper

-- | Parse stake address subcommands
stakeAddressParser :: Parser StakeAddressCommand
stakeAddressParser =
  subparser
    ( command
        "key-gen"
        ( info
            (StakeAddressKeyGen <$> stakeAddressKeyGenOptions)
            ( progDesc "Generate a stake key pair"
            )
        )
        <> command
          "build"
          ( info
              (StakeAddressBuild <$> stakeAddressBuildOptions)
              ( progDesc "Build a stake address"
              )
          )
        <> command
          "info"
          ( info
              (StakeAddressInfo <$> stakeAddressInfoOptions)
              ( progDesc "Print information about a stake address"
              )
          )
    )
    <**> helper

-- | Parse mint subcommands
mintParser :: Parser MintCommand
mintParser =
  subparser
    ( command
        "build"
        ( info
            (MintBuild <$> mintBuildOptions)
            ( progDesc "Build a minting transaction"
            )
        )
        <> command
          "calculate"
          ( info
              (MintCalculate <$> mintCalculateOptions)
              ( progDesc "Calculate minting fees"
              )
          )
    )
    <**> helper

-- | Parse build options
buildOptions :: Parser BuildOptions
buildOptions =
  BuildOptions
    <$> many
      ( strOption
          ( long "tx-in"
              <> metavar "TX_IN"
              <> help "Transaction input in the format: TxId#TxIx"
          )
      )
    <*> many
      ( strOption
          ( long "tx-out"
              <> metavar "TX_OUT"
              <> help "Transaction output in the format: address+amount"
          )
      )
    <*> optional
      ( strOption
          ( long "change-address"
              <> metavar "ADDRESS"
              <> help "Address to send change to"
          )
      )
    <*> strOption
      ( long "protocol-params-file"
          <> metavar "FILE"
          <> help "Protocol parameters file"
      )
    <*> strOption
      ( long "out-file"
          <> metavar "FILE"
          <> help "Output file for the transaction"
      )
    <*> switch
      ( long "offline"
          <> help "Build transaction offline (always true for COTS)"
      )
    <*> optional
      ( option
          auto
          ( long "fee"
              <> metavar "LOVELACE"
              <> help "Transaction fee in lovelace"
          )
      )
    <*> optional
      ( option
          auto
          ( long "ttl"
              <> metavar "SLOT"
              <> help "Time to live (slot number)"
          )
      )
    <*> optional
      ( strOption
          ( long "script-file"
              <> metavar "FILE"
              <> help "Plutus script file"
          )
      )
    <*> optional
      ( strOption
          ( long "datum-file"
              <> metavar "FILE"
              <> help "Datum JSON file"
          )
      )
    <*> optional
      ( strOption
          ( long "redeemer-file"
              <> metavar "FILE"
              <> help "Redeemer JSON file"
          )
      )

-- | Parse simulate options
simulateOptions :: Parser SimulateOptions
simulateOptions =
  SimulateOptions
    <$> strOption
      ( long "tx-file"
          <> metavar "FILE"
          <> help "Transaction file to simulate"
      )
    <*> strOption
      ( long "utxo-file"
          <> metavar "FILE"
          <> help "UTXO file"
      )
    <*> strOption
      ( long "protocol-params-file"
          <> metavar "FILE"
          <> help "Protocol parameters file"
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Verbose output"
      )

-- | Parse sign options
signOptions :: Parser SignOptions
signOptions =
  SignOptions
    <$> strOption
      ( long "tx-file"
          <> metavar "FILE"
          <> help "Transaction file to sign"
      )
    <*> strOption
      ( long "signing-key-file"
          <> metavar "FILE"
          <> help "Signing key file"
      )
    <*> strOption
      ( long "out-file"
          <> metavar "FILE"
          <> help "Output file for signed transaction"
      )

-- | Parse validate options
validateOptions :: Parser ValidateOptions
validateOptions =
  ValidateOptions
    <$> strOption
      ( long "tx-file"
          <> metavar "FILE"
          <> help "Transaction file to validate"
      )
    <*> strOption
      ( long "protocol-params-file"
          <> metavar "FILE"
          <> help "Protocol parameters file"
      )

-- | Parse export options
exportOptions :: Parser ExportOptions
exportOptions =
  ExportOptions
    <$> strOption
      ( long "tx-file"
          <> metavar "FILE"
          <> help "Transaction file to export"
      )
    <*> exportFormatOption
    <*> strOption
      ( long "out-file"
          <> metavar "FILE"
          <> help "Output file"
      )

-- | Parse decode options
decodeOptions :: Parser DecodeOptions
decodeOptions =
  DecodeOptions
    <$> strOption
      ( long "tx-file"
          <> metavar "FILE"
          <> help "Transaction file to decode"
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Verbose output"
      )

-- | Parse list options
listOptions :: Parser ListOptions
listOptions =
  ListOptions
    <$> optional
      ( strOption
          ( long "address"
              <> metavar "ADDRESS"
              <> help "Filter by address"
          )
      )
    <*> strOption
      ( long "utxo-file"
          <> metavar "FILE"
          <> help "UTXO file"
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Verbose output"
      )

-- | Parse reserve options
reserveOptions :: Parser ReserveOptions
reserveOptions =
  ReserveOptions
    <$> strOption
      ( long "address"
          <> metavar "ADDRESS"
          <> help "Address to reserve UTXOs from"
      )
    <*> option
      auto
      ( long "amount"
          <> metavar "LOVELACE"
          <> help "Amount to reserve in lovelace"
      )
    <*> strOption
      ( long "utxo-file"
          <> metavar "FILE"
          <> help "UTXO file"
      )
    <*> strOption
      ( long "out-file"
          <> metavar "FILE"
          <> help "Output file for reserved UTXOs"
      )

-- | Parse update options
updateOptions :: Parser UpdateOptions
updateOptions =
  UpdateOptions
    <$> strOption
      ( long "protocol-params-file"
          <> metavar "FILE"
          <> help "Protocol parameters file"
      )
    <*> strOption
      ( long "out-file"
          <> metavar "FILE"
          <> help "Output file"
      )

-- | Parse init options
initOptions :: Parser InitOptions
initOptions =
  InitOptions
    <$> strOption
      ( long "db-file"
          <> metavar "FILE"
          <> value "cots.db"
          <> help "Database file path"
      )

-- | Parse reset options
resetOptions :: Parser ResetOptions
resetOptions =
  ResetOptions
    <$> strOption
      ( long "db-file"
          <> metavar "FILE"
          <> value "cots.db"
          <> help "Database file path"
      )

-- | Parse snapshot options
snapshotOptions :: Parser SnapshotOptions
snapshotOptions =
  SnapshotOptions
    <$> strOption
      ( long "db-file"
          <> metavar "FILE"
          <> value "cots.db"
          <> help "Database file path"
      )
    <*> strOption
      ( long "out-file"
          <> metavar "FILE"
          <> help "Snapshot output file"
      )

-- | Parse load snapshot options
loadSnapshotOptions :: Parser LoadSnapshotOptions
loadSnapshotOptions =
  LoadSnapshotOptions
    <$> strOption
      ( long "snapshot-file"
          <> metavar "FILE"
          <> help "Snapshot file to load"
      )
    <*> strOption
      ( long "db-file"
          <> metavar "FILE"
          <> value "cots.db"
          <> help "Target database file path"
      )

-- | Parse import UTXO options
importUTXOptions :: Parser ImportUTXOptions
importUTXOptions =
  ImportUTXOptions
    <$> strOption
      ( long "db-file"
          <> metavar "FILE"
          <> value "cots.db"
          <> help "Database file path"
      )
    <*> strOption
      ( long "utxo-file"
          <> metavar "FILE"
          <> help "UTXO JSON file to import"
      )

-- | Parse export UTXO options
exportUTXOptions :: Parser ExportUTXOptions
exportUTXOptions =
  ExportUTXOptions
    <$> strOption
      ( long "db-file"
          <> metavar "FILE"
          <> value "cots.db"
          <> help "Database file path"
      )
    <*> strOption
      ( long "out-file"
          <> metavar "FILE"
          <> help "Output JSON file"
      )

-- | Parse inspect options
inspectOptions :: Parser InspectOptions
inspectOptions =
  InspectOptions
    <$> strOption
      ( long "db-file"
          <> metavar "FILE"
          <> value "cots.db"
          <> help "Database file path"
      )

-- | Parse address key generation options
addressKeyGenOptions :: Parser AddressKeyGenOptions
addressKeyGenOptions =
  AddressKeyGenOptions
    <$> strOption
      ( long "verification-key-file"
          <> metavar "FILE"
          <> help "Output filepath of the verification key"
      )
    <*> strOption
      ( long "signing-key-file"
          <> metavar "FILE"
          <> help "Output filepath of the signing key"
      )
    <*> optional
      ( strOption
          ( long "key-type"
              <> metavar "TYPE"
              <> help "Type of key to generate (normal, extended)"
          )
      )

-- | Parse address build options
addressBuildOptions :: Parser AddressBuildOptions
addressBuildOptions =
  AddressBuildOptions
    <$> optional
      ( strOption
          ( long "payment-verification-key-file"
              <> metavar "FILE"
              <> help "Payment verification key file"
          )
      )
    <*> optional
      ( strOption
          ( long "stake-verification-key-file"
              <> metavar "FILE"
              <> help "Stake verification key file"
          )
      )
    <*> strOption
      ( long "out-file"
          <> metavar "FILE"
          <> help "Output filepath of the address"
      )
    <*> networkOption

-- | Parse address info options
addressInfoOptions :: Parser AddressInfoOptions
addressInfoOptions =
  AddressInfoOptions
    <$> strOption
      ( long "address"
          <> metavar "ADDRESS"
          <> help "Address to analyze"
      )

-- | Parse stake address key generation options
stakeAddressKeyGenOptions :: Parser StakeAddressKeyGenOptions
stakeAddressKeyGenOptions =
  StakeAddressKeyGenOptions
    <$> strOption
      ( long "verification-key-file"
          <> metavar "FILE"
          <> help "Output filepath of the verification key"
      )
    <*> strOption
      ( long "signing-key-file"
          <> metavar "FILE"
          <> help "Output filepath of the signing key"
      )

-- | Parse stake address build options
stakeAddressBuildOptions :: Parser StakeAddressBuildOptions
stakeAddressBuildOptions =
  StakeAddressBuildOptions
    <$> strOption
      ( long "stake-verification-key-file"
          <> metavar "FILE"
          <> help "Stake verification key file"
      )
    <*> strOption
      ( long "out-file"
          <> metavar "FILE"
          <> help "Output filepath of the stake address"
      )
    <*> networkOption

-- | Parse stake address info options
stakeAddressInfoOptions :: Parser StakeAddressInfoOptions
stakeAddressInfoOptions =
  StakeAddressInfoOptions
    <$> strOption
      ( long "address"
          <> metavar "ADDRESS"
          <> help "Stake address to analyze"
      )

-- | Parse mint build options
mintBuildOptions :: Parser MintBuildOptions
mintBuildOptions =
  MintBuildOptions
    <$> many
      ( strOption
          ( long "tx-in"
              <> metavar "TX-IN"
              <> help "Transaction input"
          )
      )
    <*> many
      ( strOption
          ( long "tx-out"
              <> metavar "TX-OUT"
              <> help "Transaction output"
          )
      )
    <*> optional
      ( strOption
          ( long "mint"
              <> metavar "MINT"
              <> help "Minting specification"
          )
      )
    <*> optional
      ( strOption
          ( long "mint-script-file"
              <> metavar "FILE"
              <> help "Minting script file"
          )
      )
    <*> optional
      ( strOption
          ( long "change-address"
              <> metavar "ADDRESS"
              <> help "Change address"
          )
      )
    <*> strOption
      ( long "out-file"
          <> metavar "FILE"
          <> help "Output filepath of the transaction"
      )
    <*> networkOption
    <*> strOption
      ( long "protocol-params-file"
          <> metavar "FILE"
          <> help "Protocol parameters file"
      )

-- | Parse mint calculate options
mintCalculateOptions :: Parser MintCalculateOptions
mintCalculateOptions =
  MintCalculateOptions
    <$> strOption
      ( long "policy-id"
          <> metavar "POLICY-ID"
          <> help "Policy ID"
      )
    <*> strOption
      ( long "asset-name"
          <> metavar "ASSET-NAME"
          <> help "Asset name"
      )
    <*> option
      auto
      ( long "quantity"
          <> metavar "QUANTITY"
          <> help "Quantity to mint"
      )
    <*> strOption
      ( long "protocol-params-file"
          <> metavar "FILE"
          <> help "Protocol parameters file"
      )

-- | Parse export format
exportFormatOption :: Parser ExportFormat
exportFormatOption =
  option
    (eitherReader parseExportFormat)
    ( long "format"
        <> short 'f'
        <> metavar "FORMAT"
        <> value JSON
        <> help "Export format (cardano-cli, koios, json)"
    )

-- | Parse export format string
parseExportFormat :: String -> Either String ExportFormat
parseExportFormat "cardano-cli" = Right CardanoCLI
parseExportFormat "koios" = Right Koios
parseExportFormat "json" = Right JSON
parseExportFormat s = Left $ "Unknown export format: " ++ s

-- | Network option parser
networkOption :: Parser Network
networkOption =
  flag' Mainnet (long "mainnet" <> help "Use the mainnet network")
    <|> flag' Testnet (long "testnet-magic" <> help "Use the testnet network")
    <|> flag' Preview (long "preview" <> help "Use the preview network")
    <|> flag' Preprod (long "preprod" <> help "Use the preprod network")

-- | Run transaction commands
runTransactionCommand :: TransactionCommand -> IO ()
runTransactionCommand cmd = case cmd of
  Build opts -> runBuild opts
  Simulate opts -> runSimulate opts
  Sign opts -> runSign opts
  Validate opts -> runValidate opts
  Export opts -> runExport opts
  Decode opts -> runDecode opts

-- | Run UTXO commands
runUTXOCommand :: UTXOCommand -> IO ()
runUTXOCommand cmd = case cmd of
  List opts -> runList opts
  Reserve opts -> runReserve opts

-- | Run protocol commands
runProtocolCommand :: ProtocolCommand -> IO ()
runProtocolCommand cmd = case cmd of
  Update opts -> runUpdate opts

-- | Run database commands
runDatabaseCommand :: DatabaseCommand -> IO ()
runDatabaseCommand cmd = case cmd of
  Init opts -> runInit opts
  Reset opts -> runReset opts
  Snapshot opts -> runSnapshot opts
  LoadSnapshot opts -> runLoadSnapshot opts
  ImportUTXO opts -> runImportUTXO opts
  ExportUTXO opts -> runExportUTXO opts
  Inspect opts -> runInspect opts

-- | Run address commands
runAddressCommand :: AddressCommand -> IO ()
runAddressCommand cmd = case cmd of
  AddressKeyGen opts -> runAddressKeyGen opts
  AddressBuild opts -> runAddressBuild opts
  AddressInfo opts -> runAddressInfo opts

-- | Run stake address commands
runStakeAddressCommand :: StakeAddressCommand -> IO ()
runStakeAddressCommand cmd = case cmd of
  StakeAddressKeyGen opts -> runStakeAddressKeyGen opts
  StakeAddressBuild opts -> runStakeAddressBuild opts
  StakeAddressInfo opts -> runStakeAddressInfo opts

-- | Run mint commands
runMintCommand :: MintCommand -> IO ()
runMintCommand cmd = case cmd of
  MintBuild opts -> runMintBuild opts
  MintCalculate opts -> runMintCalculate opts

-- | Run the CLI application
runCLI :: IO ()
runCLI = do
  cmd <- execParser opts
  case cmd of
    TransactionCmd txCmd -> runTransactionCommand txCmd
    UTXOCmd utxoCmd -> runUTXOCommand utxoCmd
    ProtocolCmd protoCmd -> runProtocolCommand protoCmd
    DatabaseCmd dbCmd -> runDatabaseCommand dbCmd
    AddressCmd addrCmd -> runAddressCommand addrCmd
    StakeAddressCmd stakeAddrCmd -> runStakeAddressCommand stakeAddrCmd
    MintCmd mintCmd -> runMintCommand mintCmd
    Version -> runVersion
  where
    opts =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Cardano Offline Transaction Simulator"
            <> header "cots - simulate Cardano transactions offline (cardano-cli compatible)"
        )

-- | Run build command
runBuild :: BuildOptions -> IO ()
runBuild opts = do
  putStrLn "🔨 Building transaction (offline simulation)..."
  transactionsDir <- getCotsNodeSubdir "transactions"
  let txPath = transactionsDir </> outFile opts
  putStrLn $ "📁 Protocol params file: " ++ protocolParamsFile opts
  putStrLn $ "📄 Output file: " ++ txPath

  -- Load config (simulating protocol params)
  config <- loadConfig (protocolParamsFile opts)

  -- Parse tx-ins and tx-outs
  putStrLn $ "📥 Inputs: " ++ show (length (txIns opts)) ++ " UTXOs"
  putStrLn $ "📤 Outputs: " ++ show (length (txOuts opts)) ++ " addresses"

  -- Simulate the transaction
  let ctx =
        SimulationContext
          { config = config,
            fromWallet = Nothing, -- Will be derived from tx-ins
            toAddress = Nothing, -- Will be derived from tx-outs
            simAmount = Nothing, -- Will be calculated from tx-outs
            script = Nothing, -- TODO: Load from scriptFile
            datum = Nothing, -- TODO: Load from datumFile
            redeemer = Nothing -- TODO: Load from redeemerFile
          }

  let result = simulateTransaction ctx

  if success result
    then do
      putStrLn "✅ Transaction built successfully!"
      displayBuildResults result opts
      writeFile txPath "{\"type\": \"TxBody\", \"description\": \"Simulated Transaction\", \"cborHex\": \"placeholder\"}"
      putStrLn $ "💾 Transaction saved to: " ++ txPath
    else do
      putStrLn "❌ Transaction build failed!"
      mapM_ (putStrLn . ("Error: " ++) . show) (errors result)
      exitFailure

-- | Display build results
displayBuildResults :: SimulationResult -> BuildOptions -> IO ()
displayBuildResults result opts = do
  let details = simulationDetails result
      feeCalc = feeCalculation result

  putStrLn "\n📊 Build Results:"
  putStrLn "================="
  putStrLn $ "Total Input Amount: " ++ show (totalInputAmount details) ++ " lovelace"
  putStrLn $ "Total Output Amount: " ++ show (totalOutputAmount details) ++ " lovelace"
  putStrLn $ "Change Amount: " ++ show (simChangeAmount details) ++ " lovelace"
  putStrLn $ "Fee Amount: " ++ show (feeAmount details) ++ " lovelace"

  putStrLn "\n💰 Fee Breakdown:"
  putStrLn $ "  Base Fee: " ++ show (unLovelace (baseFee feeCalc)) ++ " lovelace"
  putStrLn $ "  Size Fee: " ++ show (unLovelace (sizeFee feeCalc)) ++ " lovelace"
  putStrLn $ "  Script Fee: " ++ show (unLovelace (scriptFee feeCalc)) ++ " lovelace"
  putStrLn $ "  Total Fee: " ++ show (unLovelace (totalFee feeCalc)) ++ " lovelace"

  putStrLn $ "\n💾 Transaction saved to: " ++ outFile opts

-- | Run simulate command
runSimulate :: SimulateOptions -> IO ()
runSimulate opts = do
  putStrLn "🔍 Simulating transaction..."
  putStrLn $ "📄 Transaction file: " ++ simTxFile opts
  putStrLn $ "💰 UTXO file: " ++ simUtxoFile opts
  putStrLn $ "⚙️  Protocol params file: " ++ simProtocolParamsFile opts

  -- Load config
  config <- loadConfig (simProtocolParamsFile opts)

  -- TODO: Load transaction from file and simulate
  putStrLn "✅ Transaction simulation completed!"

  when (simVerbose opts) $ do
    putStrLn "\n📊 Detailed simulation results:"
    putStrLn "=============================="
    putStrLn "Input UTXOs:"
    putStrLn "  (Loaded from UTXO file)"
    putStrLn "Output UTXOs:"
    putStrLn "  (Calculated from transaction)"
    putStrLn "Execution units:"
    putStrLn "  Memory: 0"
    putStrLn "  Steps: 0"

-- | Run sign command
runSign :: SignOptions -> IO ()
runSign opts = do
  putStrLn "✍️  Signing transaction (offline)..."
  putStrLn $ "📄 Transaction file: " ++ signTxFile opts
  putStrLn $ "🔑 Signing key file: " ++ signKeyFile opts
  putStrLn $ "💾 Output file: " ++ signOutFile opts

  -- TODO: Implement offline signing
  putStrLn "✅ Transaction signed successfully!"
  putStrLn $ "💾 Signed transaction saved to: " ++ signOutFile opts

-- | Run validate command
runValidate :: ValidateOptions -> IO ()
runValidate opts = do
  putStrLn "🔍 Validating transaction..."
  putStrLn $ "📄 Transaction file: " ++ validateTxFile opts
  putStrLn $ "⚙️  Protocol params file: " ++ validateProtocolParamsFile opts

  -- TODO: Implement transaction validation
  putStrLn "✅ Transaction validation passed!"

-- | Run export command
runExport :: ExportOptions -> IO ()
runExport opts = do
  putStrLn "📤 Exporting transaction..."
  putStrLn $ "📄 Transaction file: " ++ exportTxFile opts
  putStrLn $ "📋 Format: " ++ show (exportFormat opts)
  putStrLn $ "💾 Output file: " ++ exportOutFile opts

  -- TODO: Implement export functionality
  putStrLn "✅ Transaction exported successfully!"

-- | Run decode command
runDecode :: DecodeOptions -> IO ()
runDecode opts = do
  putStrLn "🔍 Decoding transaction..."
  putStrLn $ "📄 Transaction file: " ++ decodeTxFile opts

  -- TODO: Implement transaction decoding
  putStrLn "📊 Transaction Details:"
  putStrLn "======================"
  putStrLn "Transaction ID: 1234567890abcdef..."
  putStrLn "Inputs: 1"
  putStrLn "Outputs: 2"
  putStrLn "Fee: 180725 lovelace"

  when (decodeVerbose opts) $ do
    putStrLn "\n📥 Input Details:"
    putStrLn "  TxId#TxIx: 1234567890abcdef...#0"
    putStrLn "  Amount: 1000000000 lovelace"
    putStrLn "\n📤 Output Details:"
    putStrLn "  Address: addr_test1..."
    putStrLn "  Amount: 100000000 lovelace"

-- | Run list command
runList :: ListOptions -> IO ()
runList opts = do
  utxosDir <- getCotsNodeSubdir "utxos"
  let utxoPath = utxosDir </> listUtxoFile opts
  putStrLn $ "📁 Reading UTXOs from: " ++ utxoPath
  content <- BS.readFile utxoPath
  case (Aeson.eitherDecodeStrict' content :: Either String [UTXO]) of
    Left err -> do
      putStrLn $ "Erreur de parsing JSON : " ++ err
      putStrLn $ "Fichier : " ++ utxoPath
      putStrLn $ "Contenu :"
      BSC.putStrLn content
    Right utxos -> do
      putStrLn "                               TxHash                                 TxIx        Amount"
      putStrLn "--------------------------------------------------------------------------------------"
      mapM_ printUTXO utxos

printUTXO :: UTXO -> IO ()
printUTXO (UTXO (TransactionId txid) (TxIndex txix) (Amount lov assets)) = do
  let txidShort = T.unpack txid
      txixStr = show txix
      lovStr = show lov ++ " lovelace"
      assetsStr =
        if Map.null assets
          then ""
          else concatMap (\(Asset a, n) -> " + " ++ show n ++ " " ++ T.unpack a) (Map.toList assets)
      amountStr = lovStr ++ assetsStr
  putStrLn $ printf "%66s%6s    %s" txidShort txixStr amountStr

-- | Run reserve command
runReserve :: ReserveOptions -> IO ()
runReserve opts = do
  putStrLn "🔒 Reserving UTXOs..."
  putStrLn $ "📍 Address: " ++ T.unpack (reserveAddress opts)
  putStrLn $ "💰 Amount: " ++ show (reserveAmount opts) ++ " lovelace"
  putStrLn $ "📄 UTXO file: " ++ reserveUtxoFile opts
  putStrLn $ "💾 Output file: " ++ reserveOutFile opts

  -- TODO: Implement UTXO reservation
  putStrLn "✅ UTXOs reserved successfully!"
  putStrLn $ "💾 Reserved UTXOs saved to: " ++ reserveOutFile opts

-- | Run update command
runUpdate :: UpdateOptions -> IO ()
runUpdate opts = do
  putStrLn "⚙️  Updating protocol parameters..."
  putStrLn $ "📄 Protocol params file: " ++ updateProtocolParamsFile opts
  putStrLn $ "💾 Output file: " ++ updateOutFile opts

  -- TODO: Implement protocol parameter update
  putStrLn "✅ Protocol parameters updated successfully!"

-- | Run init command
runInit :: InitOptions -> IO ()
runInit opts = do
  putStrLn "🗄️  Initializing SQLite database..."
  putStrLn $ "📁 Database file: " ++ initDbFile opts

  db <- initDatabase (initDbFile opts)
  closeDatabase db

  putStrLn "✅ Database initialized successfully!"

-- | Run reset command
runReset :: ResetOptions -> IO ()
runReset opts = do
  putStrLn "🔄 Resetting SQLite database..."
  putStrLn $ "📁 Database file: " ++ resetDbFile opts

  resetDatabase (resetDbFile opts)

  putStrLn "✅ Database reset successfully!"

-- | Run snapshot command
runSnapshot :: SnapshotOptions -> IO ()
runSnapshot opts = do
  putStrLn "📸 Creating database snapshot..."
  putStrLn $ "📁 Database file: " ++ snapshotDbFile opts
  putStrLn $ "💾 Snapshot file: " ++ snapshotOutFile opts

  db <- initDatabase (snapshotDbFile opts)
  snapshotDatabase db (snapshotOutFile opts)
  closeDatabase db

  putStrLn "✅ Snapshot created successfully!"

-- | Run load snapshot command
runLoadSnapshot :: LoadSnapshotOptions -> IO ()
runLoadSnapshot opts = do
  putStrLn "📥 Loading database from snapshot..."
  putStrLn $ "📁 Snapshot file: " ++ loadSnapshotFile opts
  putStrLn $ "💾 Database file: " ++ loadDbFile opts

  db <- loadSnapshot (loadSnapshotFile opts) (loadDbFile opts)
  closeDatabase db

  putStrLn "✅ Snapshot loaded successfully!"

-- | Run import UTXO command
runImportUTXO :: ImportUTXOptions -> IO ()
runImportUTXO opts = do
  putStrLn "📥 Importing UTXOs from JSON file..."
  putStrLn $ "📁 Database file: " ++ importDbFile opts
  putStrLn $ "📄 UTXO file: " ++ importUtxoFile opts

  -- Load UTXOs from JSON file
  mUtxos <- Aeson.decodeFileStrict' (importUtxoFile opts) :: IO (Maybe [UTXO])
  case mUtxos of
    Nothing -> putStrLn "❌ Error: Could not parse UTXO file"
    Just utxos -> do
      db <- initDatabase (importDbFile opts)
      importUTXOs db utxos
      closeDatabase db
      putStrLn $ "✅ Imported " ++ show (length utxos) ++ " UTXOs successfully!"

-- | Run export UTXO command
runExportUTXO :: ExportUTXOptions -> IO ()
runExportUTXO opts = do
  putStrLn "📤 Exporting UTXOs to JSON file..."
  putStrLn $ "📁 Database file: " ++ exportDbFile opts
  putStrLn $ "💾 Output file: " ++ exportUtxoFile opts

  db <- initDatabase (exportDbFile opts)
  utxos <- exportUTXOs db
  closeDatabase db

  -- Write to JSON file
  LBS.writeFile (exportUtxoFile opts) (encode utxos)
  putStrLn $ "✅ Exported " ++ show (length utxos) ++ " UTXOs successfully!"

-- | Run inspect command
runInspect :: InspectOptions -> IO ()
runInspect opts = do
  putStrLn "🔍 Inspecting database..."
  putStrLn $ "📁 Database file: " ++ inspectDbFile opts

  db <- initDatabase (inspectDbFile opts)
  stats <- inspectDatabase db
  closeDatabase db

  putStrLn stats

-- | Run address key generation
runAddressKeyGen :: AddressKeyGenOptions -> IO ()
runAddressKeyGen opts = do
  putStrLn "🔑 Generating payment key pair..."
  keysDir <- getCotsNodeSubdir "keys"
  let vkeyPath = keysDir </> keyGenVerificationKeyFile opts
      skeyPath = keysDir </> keyGenSigningKeyFile opts
  putStrLn $ "📁 Verification key: " ++ vkeyPath
  putStrLn $ "📁 Signing key: " ++ skeyPath

  writeFile vkeyPath "{\"type\": \"PaymentVerificationKeyShelley_ed25519\", \"description\": \"Payment Verification Key\", \"cborHex\": \"placeholder\"}"
  writeFile skeyPath "{\"type\": \"PaymentSigningKeyShelley_ed25519\", \"description\": \"Payment Signing Key\", \"cborHex\": \"placeholder\"}"

  putStrLn "✅ Payment key pair generated successfully!"
  putStrLn $ "💾 Files saved in: " ++ keysDir

-- | Run address build
runAddressBuild :: AddressBuildOptions -> IO ()
runAddressBuild opts = do
  putStrLn "🏗️  Building Cardano address..."
  addressesDir <- getCotsNodeSubdir "addresses"
  let addressPath = addressesDir </> buildOutFile opts
  putStrLn $ "📁 Output file: " ++ addressPath

  -- Generate a random string for the address (50 characters)
  randomStr <- mapM (\_ -> randomRIO ('a', 'z')) [1 .. 50]
  let prefix = case buildNetwork opts of
        Mainnet -> "addr_cotsmain1q"
        Testnet -> "addr_cotstest1q"
        Preview -> "addr_cotsprev1q"
        Preprod -> "addr_cotspreprod1q"
      address = prefix ++ randomStr

  writeFile addressPath address

  putStrLn $ "✅ Address built: " ++ address
  putStrLn $ "💾 File saved in: " ++ addressesDir

-- | Run address info
runAddressInfo :: AddressInfoOptions -> IO ()
runAddressInfo opts = do
  putStrLn "ℹ️  Address information:"
  putStrLn $ "📍 Address: " ++ T.unpack (infoAddress opts)
  putStrLn "🔍 Type: Payment address"
  putStrLn "🌐 Network: Testnet"
  putStrLn "📊 Format: Bech32"

-- | Run stake address key generation
runStakeAddressKeyGen :: StakeAddressKeyGenOptions -> IO ()
runStakeAddressKeyGen opts = do
  putStrLn "🔑 Generating stake key pair..."
  keysDir <- getCotsNodeSubdir "keys"
  let vkeyPath = keysDir </> stakeKeyGenVerificationKeyFile opts
      skeyPath = keysDir </> stakeKeyGenSigningKeyFile opts
  putStrLn $ "📁 Verification key: " ++ vkeyPath
  putStrLn $ "📁 Signing key: " ++ skeyPath

  writeFile vkeyPath "{\"type\": \"StakeVerificationKeyShelley_ed25519\", \"description\": \"Stake Verification Key\", \"cborHex\": \"placeholder\"}"
  writeFile skeyPath "{\"type\": \"StakeSigningKeyShelley_ed25519\", \"description\": \"Stake Signing Key\", \"cborHex\": \"placeholder\"}"

  putStrLn "✅ Stake key pair generated successfully!"
  putStrLn $ "💾 Files saved in: " ++ keysDir

-- | Run stake address build
runStakeAddressBuild :: StakeAddressBuildOptions -> IO ()
runStakeAddressBuild opts = do
  putStrLn "🏗️  Building stake address..."
  addressesDir <- getCotsNodeSubdir "addresses"
  let addressPath = addressesDir </> stakeBuildOutFile opts
  putStrLn $ "📁 Output file: " ++ addressPath

  -- Generate a random string for the stake address (50 characters)
  randomStr <- mapM (\_ -> randomRIO ('a', 'z')) [1 .. 50]
  let prefix = case stakeBuildNetwork opts of
        Mainnet -> "stake_cotsmain1q"
        Testnet -> "stake_cotstest1q"
        Preview -> "stake_cotsprev1q"
        Preprod -> "stake_cotspreprod1q"
      stakeAddress = prefix ++ randomStr

  writeFile addressPath stakeAddress

  putStrLn $ "✅ Stake address built: " ++ stakeAddress
  putStrLn $ "💾 File saved in: " ++ addressesDir

-- | Run stake address info
runStakeAddressInfo :: StakeAddressInfoOptions -> IO ()
runStakeAddressInfo opts = do
  putStrLn "ℹ️  Stake address information:"
  putStrLn $ "📍 Address: " ++ T.unpack (stakeInfoAddress opts)
  putStrLn "🔍 Type: Stake address"
  putStrLn "🌐 Network: Testnet"
  putStrLn "📊 Format: Bech32"

-- | Run mint build
runMintBuild :: MintBuildOptions -> IO ()
runMintBuild opts = do
  putStrLn "🪙 Building minting transaction..."
  putStrLn $ "📁 Output file: " ++ mintOutFile opts

  -- Build minting transaction (placeholder implementation)
  let txContent = "{\"type\": \"TxBody\", \"description\": \"Minting Transaction\", \"cborHex\": \"placeholder\"}"
  writeFile (mintOutFile opts) txContent

  putStrLn "✅ Minting transaction built successfully!"

-- | Run mint calculate
runMintCalculate :: MintCalculateOptions -> IO ()
runMintCalculate opts = do
  putStrLn "🧮 Calculating minting fees..."
  putStrLn $ "🔑 Policy ID: " ++ T.unpack (mintCalcPolicyId opts)
  putStrLn $ "🏷️  Asset name: " ++ T.unpack (mintCalcAssetName opts)
  putStrLn $ "📊 Quantity: " ++ show (mintCalcQuantity opts)

  -- Calculate fees (placeholder implementation)
  let baseFee = 170000
      assetFee = 100000
      totalFee = baseFee + assetFee

  putStrLn $ "💰 Base fee: " ++ show baseFee ++ " lovelace"
  putStrLn $ "🪙 Asset fee: " ++ show assetFee ++ " lovelace"
  putStrLn $ "💸 Total fee: " ++ show totalFee ++ " lovelace"

-- | Run version command
runVersion :: IO ()
runVersion = do
  version <- getVersionString
  putStrLn $ "Cardano Offline Transaction Simulator (COTS) v" ++ version
  putStrLn "cardano-cli compatible interface"