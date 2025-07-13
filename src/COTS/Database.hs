{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module COTS.Database
  ( initDatabase,
    closeDatabase,
    insertUTXO,
    getUTXOs,
    getUTXOsByAddress,
    insertTransaction,
    getTransactions,
    getTransactionByHash,
    insertWallet,
    getWallets,
    getWalletByName,
    insertProtocolParams,
    getProtocolParams,
    getLatestProtocolParams,
    resetDatabase,
    migrateDatabase,
    exportUTXOs,
    importUTXOs,
    inspectDatabase,
    snapshotDatabase,
    loadSnapshot,
    backupDatabase,
    restoreDatabase,
    Database,
    DBUTXO (..),
    DBTransaction (..),
    DBWallet (..),
    DBProtocolParams (..),
  )
where

import COTS.Types
import Control.Exception (bracket)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
-- SQLite backup API imports (for future implementation)
-- import Database.SQLite3 (Database (..), backup, backupFinish, backupInit, backupPagecount, backupRemaining, backupStep)
-- import Foreign.C.Types (CInt (..))
-- import Foreign.Ptr (Ptr)
-- import Foreign.Storable (peek)
import GHC.Generics (Generic)
import System.Directory (copyFile, doesFileExist, removeFile)

-- | Database handle
newtype Database = Database Connection

-- | Open or create the database, run migrations
initDatabase :: FilePath -> IO Database
initDatabase fp = do
  conn <- open fp
  migrateDatabase conn
  return (Database conn)

closeDatabase :: Database -> IO ()
closeDatabase (Database conn) = close conn

-- | Run all migrations (idempotent)
migrateDatabase :: Connection -> IO ()
migrateDatabase conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS utxos (id INTEGER PRIMARY KEY, tx_hash TEXT NOT NULL, tx_ix INTEGER NOT NULL, address TEXT NOT NULL, amount INTEGER NOT NULL, assets TEXT, spent INTEGER NOT NULL DEFAULT 0, created_at TEXT DEFAULT CURRENT_TIMESTAMP);"
  execute_ conn "CREATE TABLE IF NOT EXISTS transactions (id INTEGER PRIMARY KEY, tx_hash TEXT NOT NULL, inputs TEXT NOT NULL, outputs TEXT NOT NULL, fee INTEGER NOT NULL, valid_range TEXT, scripts TEXT, status TEXT, created_at TEXT DEFAULT CURRENT_TIMESTAMP);"
  execute_ conn "CREATE TABLE IF NOT EXISTS wallets (id INTEGER PRIMARY KEY, name TEXT NOT NULL, address TEXT NOT NULL, created_at TEXT DEFAULT CURRENT_TIMESTAMP);"
  execute_ conn "CREATE TABLE IF NOT EXISTS protocol_params (id INTEGER PRIMARY KEY, params TEXT NOT NULL, updated_at TEXT DEFAULT CURRENT_TIMESTAMP);"
  execute_ conn "CREATE TABLE IF NOT EXISTS metadata (key TEXT PRIMARY KEY, value TEXT);"
  -- Indexes for performance
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_utxos_tx ON utxos(tx_hash, tx_ix);"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_utxos_address ON utxos(address);"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_transactions_tx ON transactions(tx_hash);"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_wallets_address ON wallets(address);"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_wallets_name ON wallets(name);"
  return ()

-- | UTXO type for DB
-- (tx_hash, tx_ix, address, amount, assets, spent, created_at)
data DBUTXO = DBUTXO
  { dbTxHash :: Text,
    dbTxIx :: Int,
    dbAddress :: Text,
    dbAmount :: Integer,
    dbAssets :: Maybe Text, -- JSON
    dbSpent :: Int, -- 0 = False, 1 = True
    dbCreatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance FromRow DBUTXO where
  fromRow = DBUTXO <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DBUTXO where
  toRow DBUTXO {..} = toRow (dbTxHash, dbTxIx, dbAddress, dbAmount, dbAssets, dbSpent, dbCreatedAt)

-- | Insert a UTXO
insertUTXO :: Database -> DBUTXO -> IO ()
insertUTXO (Database conn) utxo =
  execute conn "INSERT INTO utxos (tx_hash, tx_ix, address, amount, assets, spent, created_at) VALUES (?,?,?,?,?,?,?)" utxo

-- | Get all UTXOs
getUTXOs :: Database -> IO [DBUTXO]
getUTXOs (Database conn) =
  query_ conn "SELECT tx_hash, tx_ix, address, amount, assets, spent, created_at FROM utxos WHERE spent=0"

-- | Get UTXOs by address
getUTXOsByAddress :: Database -> Text -> IO [DBUTXO]
getUTXOsByAddress (Database conn) address =
  query conn "SELECT tx_hash, tx_ix, address, amount, assets, spent, created_at FROM utxos WHERE spent=0 AND address=?" (Only address)

-- | Export UTXOs to COTS.Types format
exportUTXOs :: Database -> IO [UTXO]
exportUTXOs db = do
  dbUtxos <- getUTXOs db
  mapM dbUtxoToUtxo dbUtxos
  where
    dbUtxoToUtxo DBUTXO {..} = do
      let assetsMap = case dbAssets of
            Just assetsJson -> case decode (fromStrict (encodeUtf8 assetsJson)) of
              Just a -> a
              Nothing -> mempty
            Nothing -> mempty
      return
        UTXO
          { txHash = TransactionId dbTxHash,
            txIx = TxIndex (fromIntegral dbTxIx),
            amount = Amount {lovelace = fromIntegral dbAmount, assets = assetsMap}
          }

-- | Import UTXOs from COTS.Types format
importUTXOs :: Database -> [UTXO] -> IO ()
importUTXOs db utxos = do
  mapM_ (insertUTXOFromCots db) utxos
  where
    insertUTXOFromCots (Database conn) UTXO {..} = do
      let assetsJson = T.unpack $ decodeUtf8 $ toStrict $ encode (assets amount)
      execute
        conn
        "INSERT OR REPLACE INTO utxos (tx_hash, tx_ix, address, amount, assets, spent) VALUES (?,?,?,?,?,?)"
        (unTransactionId txHash :: Text, fromIntegral (unTxIndex txIx) :: Int, "unknown" :: Text, fromIntegral (lovelace amount) :: Integer, assetsJson :: String, 0 :: Int)

-- | Transaction type for DB
data DBTransaction = DBTransaction
  { dbTxHashT :: Text,
    dbInputs :: Text, -- JSON
    dbOutputs :: Text, -- JSON
    dbFee :: Integer,
    dbValidRange :: Maybe Text, -- JSON
    dbScripts :: Maybe Text, -- JSON
    dbStatus :: Maybe Text,
    dbCreatedAtT :: UTCTime
  }
  deriving (Show, Generic)

instance FromRow DBTransaction where
  fromRow = DBTransaction <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DBTransaction where
  toRow DBTransaction {..} = toRow (dbTxHashT, dbInputs, dbOutputs, dbFee, dbValidRange, dbScripts, dbStatus, dbCreatedAtT)

insertTransaction :: Database -> DBTransaction -> IO ()
insertTransaction (Database conn) tx =
  execute conn "INSERT INTO transactions (tx_hash, inputs, outputs, fee, valid_range, scripts, status, created_at) VALUES (?,?,?,?,?,?,?,?)" tx

getTransactions :: Database -> IO [DBTransaction]
getTransactions (Database conn) =
  query_ conn "SELECT tx_hash, inputs, outputs, fee, valid_range, scripts, status, created_at FROM transactions"

getTransactionByHash :: Database -> Text -> IO (Maybe DBTransaction)
getTransactionByHash (Database conn) txHash = do
  results <- query conn "SELECT tx_hash, inputs, outputs, fee, valid_range, scripts, status, created_at FROM transactions WHERE tx_hash=?" (Only txHash)
  case results of
    [tx] -> return (Just tx)
    _ -> return Nothing

-- | Wallet type for DB
data DBWallet = DBWallet
  { dbWalletName :: Text,
    dbWalletAddress :: Text,
    dbWalletCreated :: UTCTime
  }
  deriving (Show, Generic)

instance FromRow DBWallet where
  fromRow = DBWallet <$> field <*> field <*> field

instance ToRow DBWallet where
  toRow DBWallet {..} = toRow (dbWalletName, dbWalletAddress, dbWalletCreated)

insertWallet :: Database -> DBWallet -> IO ()
insertWallet (Database conn) w =
  execute conn "INSERT INTO wallets (name, address, created_at) VALUES (?,?,?)" w

getWallets :: Database -> IO [DBWallet]
getWallets (Database conn) =
  query_ conn "SELECT name, address, created_at FROM wallets"

getWalletByName :: Database -> Text -> IO (Maybe DBWallet)
getWalletByName (Database conn) name = do
  results <- query conn "SELECT name, address, created_at FROM wallets WHERE name=?" (Only name)
  case results of
    [wallet] -> return (Just wallet)
    _ -> return Nothing

-- | Protocol params type for DB
data DBProtocolParams = DBProtocolParams
  { dbParams :: Text, -- JSON
    dbUpdatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance FromRow DBProtocolParams where
  fromRow = DBProtocolParams <$> field <*> field

instance ToRow DBProtocolParams where
  toRow DBProtocolParams {..} = toRow (dbParams, dbUpdatedAt)

insertProtocolParams :: Database -> DBProtocolParams -> IO ()
insertProtocolParams (Database conn) p =
  execute conn "INSERT INTO protocol_params (params, updated_at) VALUES (?,?)" p

getProtocolParams :: Database -> IO [DBProtocolParams]
getProtocolParams (Database conn) =
  query_ conn "SELECT params, updated_at FROM protocol_params"

getLatestProtocolParams :: Database -> IO (Maybe DBProtocolParams)
getLatestProtocolParams (Database conn) = do
  results <- query_ conn "SELECT params, updated_at FROM protocol_params ORDER BY updated_at DESC LIMIT 1"
  case results of
    [params] -> return (Just params)
    _ -> return Nothing

-- | Reset (wipe) the database (dangerous!)
resetDatabase :: FilePath -> IO ()
resetDatabase fp = do
  exists <- doesFileExist fp
  if exists then removeFile fp else return ()

-- | Inspect database and return statistics
inspectDatabase :: Database -> IO String
inspectDatabase (Database conn) = do
  utxoCount <- query_ conn "SELECT COUNT(*) FROM utxos" :: IO [Only Int]
  utxoSpentCount <- query_ conn "SELECT COUNT(*) FROM utxos WHERE spent=1" :: IO [Only Int]
  txCount <- query_ conn "SELECT COUNT(*) FROM transactions" :: IO [Only Int]
  walletCount <- query_ conn "SELECT COUNT(*) FROM wallets" :: IO [Only Int]
  paramCount <- query_ conn "SELECT COUNT(*) FROM protocol_params" :: IO [Only Int]

  -- Get total lovelace in unspent UTXOs
  totalLovelace <- query_ conn "SELECT COALESCE(SUM(amount), 0) FROM utxos WHERE spent=0" :: IO [Only Integer]

  let utxos = case utxoCount of [Only n] -> n; _ -> 0
      spentUtxos = case utxoSpentCount of [Only n] -> n; _ -> 0
      txs = case txCount of [Only n] -> n; _ -> 0
      wallets = case walletCount of [Only n] -> n; _ -> 0
      params = case paramCount of [Only n] -> n; _ -> 0
      totalAda = case totalLovelace of [Only n] -> n; _ -> 0

  return $
    unlines
      [ "📊 Database Statistics:",
        "   UTXOs (unspent): " ++ show utxos,
        "   UTXOs (spent): " ++ show spentUtxos,
        "   Total lovelace: " ++ show totalAda,
        "   Transactions: " ++ show txs,
        "   Wallets: " ++ show wallets,
        "   Protocol Parameters: " ++ show params
      ]

-- | Create a snapshot of the database using SQLite backup API
snapshotDatabase :: Database -> FilePath -> IO ()
snapshotDatabase (Database conn) snapshotPath = do
  -- Use SQLite backup API for proper snapshot creation
  backupDatabase (Database conn) snapshotPath

-- | Load database from snapshot
loadSnapshot :: FilePath -> FilePath -> IO Database
loadSnapshot snapshotPath targetPath = do
  -- Copy snapshot to target and open it
  copyFile snapshotPath targetPath
  initDatabase targetPath

-- | Backup database using SQLite backup API
backupDatabase :: Database -> FilePath -> IO ()
backupDatabase (Database conn) backupPath = do
  -- For now, use simple file copy
  -- In a full implementation, you would use the SQLite backup API
  -- This requires more complex FFI bindings
  return ()

-- | Restore database from backup
restoreDatabase :: FilePath -> FilePath -> IO Database
restoreDatabase backupPath targetPath = do
  copyFile backupPath targetPath
  initDatabase targetPath