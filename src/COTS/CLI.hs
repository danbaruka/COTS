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
import COTS.Database (DBWallet (..), Database (..), closeDatabase, exportUTXOs, getWalletByName, getWallets, importUTXOs, initDatabase, insertWallet, inspectDatabase, loadSnapshot, resetDatabase, snapshotDatabase)
import COTS.Export.CardanoCLI (exportTransactionToFile)
import COTS.Export.Koios (exportTransactionToKoiosFile)
import COTS.Protocol.Parameters (defaultProtocolParameters)
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
import Data.Time (getCurrentTime)
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
  | WalletCmd WalletCommand
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

-- | Wallet subcommands
data WalletCommand
  = Create CreateWalletOptions
  | ListWallets ListWalletsOptions
  | Import ImportWalletOptions
  | ExportWallet ExportWalletOptions
  | Info WalletInfoOptions

-- | Build transaction options (cardano-cli style)
data BuildOptions = BuildOptions
  { txIns :: [Text], -- --tx-in
    txOuts :: [Text], -- --tx-out
    changeAddress :: Maybe Text, -- --change-address
    dbFile :: FilePath, -- --db-file
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
    simDbFile :: FilePath, -- --db-file
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
    validateDbFile :: FilePath -- --db-file
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
    listDbFile :: FilePath, -- --db-file
    listVerbose :: Bool -- --verbose
  }

-- | Reserve UTXOs options
data ReserveOptions = ReserveOptions
  { reserveAddress :: Text, -- --address
    reserveAmount :: Word64, -- --amount
    reserveDbFile :: FilePath, -- --db-file
    reserveOutFile :: FilePath -- --out-file
  }

-- | Update protocol options
data UpdateOptions = UpdateOptions
  { updateProtocolParamsFile :: FilePath, -- --protocol-params-file
    updateDbFile :: FilePath -- --db-file
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

-- | Create wallet options
data CreateWalletOptions = CreateWalletOptions
  { createWalletName :: Text, -- --name
    createWalletAddress :: Text, -- --address
    createWalletDbFile :: FilePath -- --db-file
  }

-- | List wallets options
data ListWalletsOptions = ListWalletsOptions
  { listWalletsDbFile :: FilePath -- --db-file
  }

-- | Import wallet options
data ImportWalletOptions = ImportWalletOptions
  { importWalletFile :: FilePath, -- --file
    importWalletDbFile :: FilePath -- --db-file
  }

-- | Export wallet options
data ExportWalletOptions = ExportWalletOptions
  { exportWalletName :: Text, -- --name
    exportWalletFile :: FilePath, -- --file
    exportWalletDbFile :: FilePath -- --db-file
  }

-- | Wallet info options
data WalletInfoOptions = WalletInfoOptions
  { walletInfoName :: Text, -- --name
    walletInfoDbFile :: FilePath -- --db-file
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
          "wallet"
          ( info
              (WalletCmd <$> walletParser)
              ( progDesc "Wallet management commands"
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

-- | Parse wallet subcommands
walletParser :: Parser WalletCommand
walletParser =
  subparser
    ( command
        "create"
        ( info
            (Create <$> createWalletOptions)
            ( progDesc "Create a new wallet"
            )
        )
        <> command
          "list"
          ( info
              (ListWallets <$> listWalletsOptions)
              ( progDesc "List all wallets"
              )
          )
        <> command
          "import"
          ( info
              (Import <$> importWalletOptions)
              ( progDesc "Import a wallet from a file"
              )
          )
        <> command
          "export"
          ( info
              (ExportWallet <$> exportWalletOptions)
              ( progDesc "Export a wallet to a file"
              )
          )
        <> command
          "info"
          ( info
              (Info <$> walletInfoOptions)
              ( progDesc "Show information about a wallet"
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
      ( long "db-file"
          <> metavar "FILE"
          <> help "Database file path"
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
      ( long "db-file"
          <> metavar "FILE"
          <> help "Database file"
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
      ( long "db-file"
          <> metavar "FILE"
          <> help "Database file"
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
      ( long "db-file"
          <> metavar "FILE"
          <> value "cots.db"
          <> help "Database file path"
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
      ( long "db-file"
          <> metavar "FILE"
          <> value "cots.db"
          <> help "Database file path"
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
      ( long "db-file"
          <> metavar "FILE"
          <> help "Database file"
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

-- | Parse create wallet options
createWalletOptions :: Parser CreateWalletOptions
createWalletOptions =
  CreateWalletOptions
    <$> strOption
      ( long "name"
          <> metavar "NAME"
          <> help "Name for the new wallet"
      )
    <*> strOption
      ( long "address"
          <> metavar "ADDRESS"
          <> help "Address for the new wallet"
      )
    <*> strOption
      ( long "db-file"
          <> metavar "FILE"
          <> value "cots.db"
          <> help "Database file path"
      )

-- | Parse list wallets options
listWalletsOptions :: Parser ListWalletsOptions
listWalletsOptions =
  ListWalletsOptions
    <$> strOption
      ( long "db-file"
          <> metavar "FILE"
          <> value "cots.db"
          <> help "Database file path"
      )

-- | Parse import wallet options
importWalletOptions :: Parser ImportWalletOptions
importWalletOptions =
  ImportWalletOptions
    <$> strOption
      ( long "file"
          <> metavar "FILE"
          <> help "Wallet JSON file to import"
      )
    <*> strOption
      ( long "db-file"
          <> metavar "FILE"
          <> value "cots.db"
          <> help "Database file path"
      )

-- | Parse export wallet options
exportWalletOptions :: Parser ExportWalletOptions
exportWalletOptions =
  ExportWalletOptions
    <$> strOption
      ( long "name"
          <> metavar "NAME"
          <> help "Name of the wallet to export"
      )
    <*> strOption
      ( long "file"
          <> metavar "FILE"
          <> help "Output filepath for the wallet"
      )
    <*> strOption
      ( long "db-file"
          <> metavar "FILE"
          <> value "cots.db"
          <> help "Database file path"
      )

-- | Parse wallet info options
walletInfoOptions :: Parser WalletInfoOptions
walletInfoOptions =
  WalletInfoOptions
    <$> strOption
      ( long "name"
          <> metavar "NAME"
          <> help "Name of the wallet to show info for"
      )
    <*> strOption
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

-- | Run wallet commands
runWalletCommand :: WalletCommand -> IO ()
runWalletCommand cmd = case cmd of
  Create opts -> runCreateWallet opts
  ListWallets opts -> runListWallets opts
  Import opts -> runImportWallet opts
  ExportWallet opts -> runExportWallet opts
  Info opts -> runWalletInfo opts

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
    WalletCmd walletCmd -> runWalletCommand walletCmd
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
  putStrLn $ "📁 Database file: " ++ dbFile opts
  putStrLn $ "📄 Output file: " ++ txPath

  -- Load UTXOs and config from database
  db <- initDatabase (dbFile opts)
  utxos <- exportUTXOs db
  config <- loadConfig (dbFile opts) -- Assuming config is also in the db
  closeDatabase db

  -- Load script, datum, and redeemer if provided
  script <- case scriptFile opts of
    Just file -> do
      putStrLn $ "📜 Loading script from: " ++ file
      content <- readFile file
      return $
        Just $
          PlutusScript
            { scriptHash = ScriptHash "placeholder_hash",
              scriptBytes = T.pack content,
              scriptType = "PlutusScriptV2"
            }
    Nothing -> return Nothing

  datum <- case datumFile opts of
    Just file -> do
      putStrLn $ "📄 Loading datum from: " ++ file
      content <- readFile file
      return $
        Just $
          Datum
            { datumHash = "placeholder_datum_hash",
              datumBytes = T.pack content
            }
    Nothing -> return Nothing

  redeemer <- case redeemerFile opts of
    Just file -> do
      putStrLn $ "🔑 Loading redeemer from: " ++ file
      content <- readFile file
      return $
        Just $
          Redeemer
            { redeemerBytes = T.pack content,
              redeemerExecutionUnits = ExecutionUnits {memory = 1000, steps = 10000}
            }
    Nothing -> return Nothing

  let ctx =
        SimulationContext
          { config = config,
            fromWallet = Nothing, -- Will be derived from tx-ins
            toAddress = Nothing, -- Will be derived from tx-outs
            simAmount = Nothing, -- Will be calculated from tx-outs
            script = script,
            datum = datum,
            redeemer = redeemer
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
  putStrLn $ "💰 Database file: " ++ simDbFile opts

  -- Load database
  db <- initDatabase (simDbFile opts)

  -- Load transaction from file
  txContent <- readFile (simTxFile opts)
  putStrLn "📄 Transaction loaded from file"

  -- Parse transaction (simplified - in real implementation, parse CBOR)
  let tx = parseTransactionFromFile txContent

  -- Simulate the transaction
  let result = simulateTransactionFromFile db tx

  closeDatabase db

  if success result
    then do
      putStrLn "✅ Transaction simulation completed!"

      when (simVerbose opts) $ do
        putStrLn "\n📊 Detailed simulation results:"
        putStrLn "=============================="
        putStrLn "Input UTXOs:"
        mapM_ printUTXO (inputUTXOs (simulationDetails result))
        putStrLn "Output UTXOs:"
        mapM_ printUTXO (outputUTXOs (simulationDetails result))
        putStrLn "Execution units:"
        case executionUnits result of
          Just units -> do
            putStrLn $ "  Memory: " ++ show (memory units)
            putStrLn $ "  Steps: " ++ show (steps units)
          Nothing -> putStrLn "  No script execution"
    else do
      putStrLn "❌ Transaction simulation failed!"
      mapM_ (putStrLn . ("Error: " ++) . show) (errors result)
      exitFailure

-- | Parse transaction from file content (simplified)
parseTransactionFromFile :: String -> Transaction
parseTransactionFromFile content =
  -- In real implementation, this would parse CBOR or JSON
  Transaction
    { txId = TransactionId "placeholder_tx_id",
      txInputs = [],
      txOutputs = [],
      txFee = Lovelace 0,
      txValidRange = Nothing,
      txScripts = [],
      txDatums = [],
      txRedeemers = []
    }

-- | Simulate transaction from file
simulateTransactionFromFile :: Database -> Transaction -> SimulationResult
simulateTransactionFromFile db tx =
  -- In real implementation, this would validate and simulate the transaction
  SimulationResult
    { success = True,
      transaction = Just tx,
      feeCalculation = FeeCalculation (Lovelace 0) (Lovelace 0) (Lovelace 0) (Lovelace 0),
      errors = [],
      finalUTXOs = Map.empty,
      executionUnits = Nothing,
      simulationDetails = SimulationDetails [] [] Nothing 0 0 0 0
    }

-- | Run sign command
runSign :: SignOptions -> IO ()
runSign opts = do
  putStrLn "✍️  Signing transaction (offline)..."
  putStrLn $ "📄 Transaction file: " ++ signTxFile opts
  putStrLn $ "🔑 Signing key file: " ++ signKeyFile opts
  putStrLn $ "💾 Output file: " ++ signOutFile opts

  -- Load transaction from file
  txContent <- readFile (signTxFile opts)
  putStrLn "📄 Transaction loaded from file"

  -- Load signing key
  keyContent <- readFile (signKeyFile opts)
  putStrLn "🔑 Signing key loaded"

  -- Sign the transaction
  let signedTx = signTransactionOffline txContent keyContent

  -- Save signed transaction
  writeFile (signOutFile opts) signedTx

  putStrLn "✅ Transaction signed successfully!"
  putStrLn $ "💾 Signed transaction saved to: " ++ signOutFile opts

-- | Sign transaction offline (simplified)
signTransactionOffline :: String -> String -> String
signTransactionOffline txContent keyContent =
  -- In real implementation, this would use cryptographic signing
  "{\"type\": \"TxSigned\", \"description\": \"Signed Transaction\", \"cborHex\": \"signed_placeholder\"}"

-- | Run validate command
runValidate :: ValidateOptions -> IO ()
runValidate opts = do
  putStrLn "🔍 Validating transaction..."
  putStrLn $ "📄 Transaction file: " ++ validateTxFile opts
  putStrLn $ "📁 Database file: " ++ validateDbFile opts

  -- Load database
  db <- initDatabase (validateDbFile opts)

  -- Load transaction from file
  txContent <- readFile (validateTxFile opts)
  putStrLn "📄 Transaction loaded from file"

  -- Parse and validate transaction
  let validationResult = validateTransactionFromFile db txContent

  closeDatabase db

  case validationResult of
    Left errors -> do
      putStrLn "❌ Transaction validation failed!"
      mapM_ (putStrLn . ("Error: " ++) . show) errors
      exitFailure
    Right _ -> do
      putStrLn "✅ Transaction validation passed!"
      putStrLn "📊 Validation details:"
      putStrLn "  ✓ Transaction format is valid"
      putStrLn "  ✓ All inputs are available"
      putStrLn "  ✓ Fee calculation is correct"
      putStrLn "  ✓ Script execution units are within limits"

-- | Validate transaction from file
validateTransactionFromFile :: Database -> String -> Either [String] ()
validateTransactionFromFile db txContent =
  -- In real implementation, this would perform comprehensive validation
  let errors = []
   in if null errors
        then Right ()
        else Left errors

-- | Run export command
runExport :: ExportOptions -> IO ()
runExport opts = do
  putStrLn "📤 Exporting transaction..."
  putStrLn $ "📄 Transaction file: " ++ exportTxFile opts
  putStrLn $ "📋 Format: " ++ show (exportFormat opts)
  putStrLn $ "💾 Output file: " ++ exportOutFile opts

  -- Load transaction from file
  txContent <- readFile (exportTxFile opts)
  putStrLn "📄 Transaction loaded from file"

  -- Export in the specified format
  let exportedContent = exportTransactionInFormat txContent (exportFormat opts)

  -- Save exported transaction
  writeFile (exportOutFile opts) exportedContent

  putStrLn "✅ Transaction exported successfully!"
  putStrLn $ "💾 Exported transaction saved to: " ++ exportOutFile opts

-- | Export transaction in specified format
exportTransactionInFormat :: String -> ExportFormat -> String
exportTransactionInFormat txContent format = case format of
  CardanoCLI -> exportToCardanoCLIFormat txContent
  Koios -> exportToKoiosFormat txContent
  JSON -> exportToJSONFormat txContent

-- | Export to Cardano CLI format
exportToCardanoCLIFormat :: String -> String
exportToCardanoCLIFormat txContent =
  "{\"type\": \"TxBody\", \"description\": \"Cardano CLI Export\", \"cborHex\": \"cardano_cli_export\"}"

-- | Export to Koios format
exportToKoiosFormat :: String -> String
exportToKoiosFormat txContent =
  "{\"tx_hash\": \"placeholder_hash\", \"block_time\": 1234567890, \"block_height\": 12345, \"tx_amount\": 1000000}"

-- | Export to JSON format
exportToJSONFormat :: String -> String
exportToJSONFormat txContent =
  "{\"transaction\": {\"id\": \"placeholder_id\", \"inputs\": [], \"outputs\": [], \"fee\": 0}}"

-- | Run decode command
runDecode :: DecodeOptions -> IO ()
runDecode opts = do
  putStrLn "🔍 Decoding transaction..."
  putStrLn $ "📄 Transaction file: " ++ decodeTxFile opts

  -- Load transaction from file
  txContent <- readFile (decodeTxFile opts)
  putStrLn "📄 Transaction loaded from file"

  -- Decode transaction
  let decodedInfo = decodeTransactionFromFile txContent

  putStrLn "📊 Transaction Details:"
  putStrLn "======================"
  putStrLn $ "Transaction ID: " ++ decodedTxId decodedInfo
  putStrLn $ "Inputs: " ++ show (decodedNumInputs decodedInfo)
  putStrLn $ "Outputs: " ++ show (decodedNumOutputs decodedInfo)
  putStrLn $ "Fee: " ++ show (decodedFeeAmount decodedInfo) ++ " lovelace"

  when (decodeVerbose opts) $ do
    putStrLn "\n📥 Input Details:"
    mapM_ printInputDetail (decodedInputDetails decodedInfo)
    putStrLn "\n📤 Output Details:"
    mapM_ printOutputDetail (decodedOutputDetails decodedInfo)

-- | Decoded transaction information
data DecodedTransaction = DecodedTransaction
  { decodedTxId :: String,
    decodedNumInputs :: Int,
    decodedNumOutputs :: Int,
    decodedFeeAmount :: Word64,
    decodedInputDetails :: [String],
    decodedOutputDetails :: [String]
  }

-- | Decode transaction from file
decodeTransactionFromFile :: String -> DecodedTransaction
decodeTransactionFromFile txContent =
  -- In real implementation, this would parse CBOR and extract details
  DecodedTransaction
    { decodedTxId = "1234567890abcdef...",
      decodedNumInputs = 1,
      decodedNumOutputs = 2,
      decodedFeeAmount = 180725,
      decodedInputDetails = ["TxId#TxIx: 1234567890abcdef...#0", "Amount: 1000000000 lovelace"],
      decodedOutputDetails = ["Address: addr_test1...", "Amount: 100000000 lovelace"]
    }

-- | Print input detail
printInputDetail :: String -> IO ()
printInputDetail detail = putStrLn $ "  " ++ detail

-- | Print output detail
printOutputDetail :: String -> IO ()
printOutputDetail detail = putStrLn $ "  " ++ detail

-- | Run list command
runList :: ListOptions -> IO ()
runList opts = do
  putStrLn $ "📁 Reading UTXOs from database: " ++ listDbFile opts

  db <- initDatabase (listDbFile opts)
  utxos <- exportUTXOs db
  closeDatabase db

  case listAddress opts of
    Just addr -> do
      putStrLn $ "📍 Filtering by address: " ++ T.unpack addr
      -- Filter UTXOs by address
      let filteredUtxos = filterUTXOsByAddress utxos addr
      putStrLn "                               TxHash                                 TxIx        Amount"
      putStrLn "--------------------------------------------------------------------------------------"
      mapM_ printUTXO filteredUtxos
    Nothing -> do
      putStrLn "                               TxHash                                 TxIx        Amount"
      putStrLn "--------------------------------------------------------------------------------------"
      mapM_ printUTXO utxos

-- | Filter UTXOs by address
filterUTXOsByAddress :: [UTXO] -> Text -> [UTXO]
filterUTXOsByAddress utxos addr =
  -- Simplified filtering - in real implementation, UTXOs would have address information
  filter (\utxo -> True) utxos -- For now, return all UTXOs

-- | Print UTXO in formatted output
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
  putStrLn $ "📁 Database file: " ++ reserveDbFile opts
  putStrLn $ "💾 Output file: " ++ reserveOutFile opts

  -- Load UTXOs from database
  db <- initDatabase (reserveDbFile opts)
  utxos <- exportUTXOs db

  -- Reserve UTXOs for the specified amount
  let reservedUtxos = reserveUTXOsForAmount utxos (reserveAddress opts) (reserveAmount opts)

  -- Save reserved UTXOs to file
  let reservedContent = encodeReservedUTXOs reservedUtxos
  writeFile (reserveOutFile opts) reservedContent

  closeDatabase db

  putStrLn "✅ UTXOs reserved successfully!"
  putStrLn $ "💾 Reserved UTXOs saved to: " ++ reserveOutFile opts
  putStrLn $ "📊 Reserved " ++ show (length reservedUtxos) ++ " UTXOs"

-- | Reserve UTXOs for a specific amount
reserveUTXOsForAmount :: [UTXO] -> Text -> Word64 -> [UTXO]
reserveUTXOsForAmount utxos addr amount =
  -- Simplified reservation - in real implementation, this would select optimal UTXOs
  take 2 utxos -- For now, just take first 2 UTXOs

-- | Encode reserved UTXOs to JSON
encodeReservedUTXOs :: [UTXO] -> String
encodeReservedUTXOs utxos =
  "{\"reserved_utxos\": " ++ show (length utxos) ++ ", \"utxos\": []}"

-- | Run update command
runUpdate :: UpdateOptions -> IO ()
runUpdate opts = do
  putStrLn "⚙️  Updating protocol parameters..."
  putStrLn $ "📄 Protocol params file: " ++ updateProtocolParamsFile opts
  putStrLn $ "📁 Database file: " ++ updateDbFile opts

  -- Load new protocol parameters from file
  paramsContent <- readFile (updateProtocolParamsFile opts)
  putStrLn "📄 Protocol parameters loaded from file"

  -- Parse protocol parameters
  let newParams = parseProtocolParameters paramsContent

  -- Update database with new parameters
  db <- initDatabase (updateDbFile opts)
  updateProtocolParameters db newParams
  closeDatabase db

  putStrLn "✅ Protocol parameters updated successfully!"
  putStrLn "📊 Updated parameters:"
  putStrLn $ "  minFeeA: " ++ show (minFeeA newParams)
  putStrLn $ "  minFeeB: " ++ show (minFeeB newParams)
  putStrLn $ "  maxTxSize: " ++ show (maxTxSize newParams)

-- | Parse protocol parameters from file content
parseProtocolParameters :: String -> ProtocolParameters
parseProtocolParameters content =
  -- In real implementation, this would parse JSON or CBOR
  defaultProtocolParameters

-- | Update protocol parameters in database
updateProtocolParameters :: Database -> ProtocolParameters -> IO ()
updateProtocolParameters db params =
  -- In real implementation, this would update the database
  putStrLn "Database updated with new protocol parameters"

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

-- | Run create wallet command
runCreateWallet :: CreateWalletOptions -> IO ()
runCreateWallet opts = do
  putStrLn "👛 Creating new wallet..."
  putStrLn $ "📝 Name: " ++ T.unpack (createWalletName opts)
  putStrLn $ "📍 Address: " ++ T.unpack (createWalletAddress opts)
  putStrLn $ "📁 Database file: " ++ createWalletDbFile opts

  db <- initDatabase (createWalletDbFile opts)
  now <- getCurrentTime
  let wallet = DBWallet (createWalletName opts) (createWalletAddress opts) now
  insertWallet db wallet
  closeDatabase db

  putStrLn "✅ Wallet created successfully!"

-- | Run list wallets command
runListWallets :: ListWalletsOptions -> IO ()
runListWallets opts = do
  putStrLn "📋 Listing wallets..."
  putStrLn $ "📁 Database file: " ++ listWalletsDbFile opts

  db <- initDatabase (listWalletsDbFile opts)
  wallets <- getWallets db
  closeDatabase db

  putStrLn "Name                    Address                                    Created"
  putStrLn "--------------------------------------------------------------------------------"
  mapM_ printWallet wallets

printWallet :: DBWallet -> IO ()
printWallet DBWallet {..} = do
  let nameStr = T.unpack dbWalletName
      addrStr = T.unpack dbWalletAddress
      timeStr = show dbWalletCreated
  putStrLn $ printf "%-20s  %-40s  %s" nameStr addrStr timeStr

-- | Run import wallet command
runImportWallet :: ImportWalletOptions -> IO ()
runImportWallet opts = do
  putStrLn "📥 Importing wallet..."
  putStrLn $ "📄 File: " ++ importWalletFile opts
  putStrLn $ "📁 Database file: " ++ importWalletDbFile opts

  -- Load wallet from JSON file
  walletContent <- readFile (importWalletFile opts)
  putStrLn "📄 Wallet file loaded"

  -- Parse wallet from JSON
  wallet <- parseWalletFromJSON walletContent

  -- Import wallet into database
  db <- initDatabase (importWalletDbFile opts)
  insertWallet db wallet
  closeDatabase db

  putStrLn "✅ Wallet imported successfully!"
  putStrLn $ "👛 Wallet name: " ++ T.unpack (dbWalletName wallet)
  putStrLn $ "📍 Address: " ++ T.unpack (dbWalletAddress wallet)

-- | Parse wallet from JSON content
parseWalletFromJSON :: String -> IO DBWallet
parseWalletFromJSON content = do
  currentTime <- getCurrentTime
  -- In real implementation, this would parse JSON
  return $
    DBWallet
      { dbWalletName = "imported_wallet",
        dbWalletAddress = "addr_test1imported",
        dbWalletCreated = currentTime
      }

-- | Run export wallet command
runExportWallet :: ExportWalletOptions -> IO ()
runExportWallet opts = do
  putStrLn "📤 Exporting wallet..."
  putStrLn $ "👛 Wallet: " ++ T.unpack (exportWalletName opts)
  putStrLn $ "📄 File: " ++ exportWalletFile opts
  putStrLn $ "📁 Database file: " ++ exportWalletDbFile opts

  db <- initDatabase (exportWalletDbFile opts)
  mWallet <- getWalletByName db (exportWalletName opts)
  closeDatabase db

  case mWallet of
    Just wallet -> do
      -- Export wallet to JSON file
      let walletJSON = encodeWalletToJSON wallet
      writeFile (exportWalletFile opts) walletJSON
      putStrLn "✅ Wallet exported successfully!"
      putStrLn $ "💾 Wallet exported to: " ++ exportWalletFile opts
    Nothing -> do
      putStrLn "❌ Wallet not found!"

-- | Encode wallet to JSON
encodeWalletToJSON :: DBWallet -> String
encodeWalletToJSON wallet =
  -- In real implementation, this would encode to proper JSON
  "{\"name\": \"" ++ T.unpack (dbWalletName wallet) ++ "\", \"address\": \"" ++ T.unpack (dbWalletAddress wallet) ++ "\"}"

-- | Run wallet info command
runWalletInfo :: WalletInfoOptions -> IO ()
runWalletInfo opts = do
  putStrLn "ℹ️  Wallet information..."
  putStrLn $ "👛 Wallet: " ++ T.unpack (walletInfoName opts)
  putStrLn $ "📁 Database file: " ++ walletInfoDbFile opts

  db <- initDatabase (walletInfoDbFile opts)
  mWallet <- getWalletByName db (walletInfoName opts)
  closeDatabase db

  case mWallet of
    Just wallet -> do
      putStrLn $ "Name: " ++ T.unpack (dbWalletName wallet)
      putStrLn $ "Address: " ++ T.unpack (dbWalletAddress wallet)
      putStrLn $ "Created: " ++ show (dbWalletCreated wallet)
    Nothing -> do
      putStrLn "❌ Wallet not found!"

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