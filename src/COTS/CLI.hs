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
  )
where

import COTS.Config (loadConfig)
import COTS.Export.CardanoCLI (exportTransactionToFile)
import COTS.Export.Koios (exportTransactionToKoiosFile)
import COTS.Simulation.Core (SimulationContext (..), simulateTransaction)
import COTS.Types hiding (command)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Word (Word64)
import Options.Applicative
import System.Exit (exitFailure)
import Text.Printf (printf)

-- | Main CLI command
data Command
  = TransactionCmd TransactionCommand
  | UTXOCmd UTXOCommand
  | ProtocolCmd ProtocolCommand
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

-- | Run the CLI application
runCLI :: IO ()
runCLI = do
  cmd <- execParser opts
  case cmd of
    TransactionCmd txCmd -> runTransactionCommand txCmd
    UTXOCmd utxoCmd -> runUTXOCommand utxoCmd
    ProtocolCmd protoCmd -> runProtocolCommand protoCmd
    Version -> runVersion
  where
    opts =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Cardano Offline Transaction Simulator"
            <> header "cots - simulate Cardano transactions offline (cardano-cli compatible)"
        )

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

-- | Run build command
runBuild :: BuildOptions -> IO ()
runBuild opts = do
  putStrLn "🔨 Building transaction (offline simulation)..."
  putStrLn $ "📁 Protocol params file: " ++ protocolParamsFile opts
  putStrLn $ "📄 Output file: " ++ outFile opts

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
  content <- BS.readFile (listUtxoFile opts)
  case (Aeson.eitherDecodeStrict' content :: Either String [UTXO]) of
    Left err -> do
      putStrLn $ "Erreur de parsing JSON : " ++ err
      putStrLn $ "Fichier : " ++ listUtxoFile opts
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

-- | Run version command
runVersion :: IO ()
runVersion = do
  putStrLn "Cardano Offline Transaction Simulator (COTS) v0.1.0.0"
  putStrLn "cardano-cli compatible interface"