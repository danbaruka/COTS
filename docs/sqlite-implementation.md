# SQLite Implementation for COTS

## 🎯 Overview

The SQLite implementation for COTS (Cardano Offline Transaction Simulator) provides persistent and robust management of the UTxO state, enabling realistic and controlled Cardano transaction simulations.

## 🏗️ Architecture

### Database Module (`src/COTS/Database.hs`)

The main module manages all database operations:

```haskell
module COTS.Database
  ( Database(..)
  , initDatabase
  , closeDatabase
  , resetDatabase
  , snapshotDatabase
  , loadSnapshot
  , importUTXOs
  , exportUTXOs
  , getUTXOs
  , addUTXO
  , removeUTXO
  , getWalletBalance
  , addTransaction
  , getTransactionHistory
  , updateProtocolParams
  , getProtocolParams
  , addScriptLog
  , getScriptLogs
  , addAsset
  , getAssets
  , inspectDatabase
  ) where
```

### Data Structure

```haskell
data Database = Database
  { dbConnection :: SQLite.Database
  , dbPath :: FilePath
  }
```

## 📊 Database Schema

### Table `utxos`

```sql
CREATE TABLE utxos (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  tx_hash TEXT NOT NULL,
  tx_ix INTEGER NOT NULL,
  address TEXT NOT NULL,
  lovelace INTEGER NOT NULL,
  assets TEXT,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  UNIQUE(tx_hash, tx_ix)
);
```

### Table `tx_history`

```sql
CREATE TABLE tx_history (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  tx_hash TEXT NOT NULL,
  tx_type TEXT NOT NULL,
  inputs TEXT,
  outputs TEXT,
  fee INTEGER,
  success BOOLEAN,
  error_message TEXT,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

### Table `wallets`

```sql
CREATE TABLE wallets (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL UNIQUE,
  address TEXT NOT NULL,
  mnemonic TEXT,
  tags TEXT,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

### Table `protocol_params`

```sql
CREATE TABLE protocol_params (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  network TEXT NOT NULL,
  params TEXT NOT NULL,
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

### Table `script_logs`

```sql
CREATE TABLE script_logs (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  script_hash TEXT NOT NULL,
  datum TEXT,
  redeemer TEXT,
  execution_units TEXT,
  success BOOLEAN,
  error_message TEXT,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

### Table `assets`

```sql
CREATE TABLE assets (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  asset_id TEXT NOT NULL UNIQUE,
  name TEXT,
  policy_id TEXT,
  metadata TEXT,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

## 🔧 Implemented Features

### 1. Database Management

- **Initialization**: `initDatabase` - Creates tables and indexes
- **Close**: `closeDatabase` - Properly closes the connection
- **Reset**: `resetDatabase` - Drops and recreates all tables

### 2. UTxO Management

- **Import**: `importUTXOs` - Loads UTxOs from JSON
- **Export**: `exportUTXOs` - Saves UTxOs to JSON
- **CRUD**: `addUTXO`, `removeUTXO`, `getUTXOs`
- **Balance**: `getWalletBalance` - Calculates the balance of an address

### 3. Snapshots and Backup

- **Snapshot**: `snapshotDatabase` - Creates a copy of the database
- **Load**: `loadSnapshot` - Restores from a snapshot

### 4. History and Logs

- **Transactions**: `addTransaction`, `getTransactionHistory`
- **Scripts**: `addScriptLog`, `getScriptLogs`
- **Assets**: `addAsset`, `getAssets`

### 5. Inspection

- **Stats**: `inspectDatabase` - Displays statistics

## 🎮 CLI Interface

### Main Commands

```bash
# Initialization
cotscli database init --db-file cots.db

# UTxO Management
cotscli database import-utxo --db-file cots.db --utxo-file utxos.json
cotscli database export-utxo --db-file cots.db --out-file exported.json

# Snapshots
cotscli database snapshot --db-file cots.db --out-file snapshot.db
cotscli database load-snapshot --snapshot-file snapshot.db --db-file restored.db

# Inspection
cotscli database inspect --db-file cots.db

# Reset
cotscli database reset --db-file cots.db
```

## 🔒 Security and Integrity

### ACID Transactions

- All critical operations use SQLite transactions
- Automatic rollback on error
- Guaranteed consistency

### Data Validation

- Uniqueness constraints checks
- JSON format validation
- Error handling for parsing

### Performance Indexes

```sql
CREATE INDEX idx_utxos_address ON utxos(address);
CREATE INDEX idx_utxos_tx_hash ON utxos(tx_hash);
CREATE INDEX idx_tx_history_tx_hash ON tx_history(tx_hash);
CREATE INDEX idx_wallets_address ON wallets(address);
```

## 📈 Performance

### Optimizations

- **Indexes** on frequently queried fields
- **Transactions** for batch operations
- **Prepared statements** for repeated operations
- **JSON** for complex data (assets, metadata)

### Expected Metrics

- **Read**: ~1000 UTxOs/second
- **Write**: ~100 UTxOs/second
- **Snapshot**: ~1MB/second
- **Query**: <10ms per address

## 🧪 Testing

### Unit Tests

- JSON serialization of UTxOs
- Data format validation
- Error handling

### Integration Tests

- Full import/export cycle
- Snapshots and restore
- Performance with large volumes

### Regression Tests

- Format compatibility
- Schema migration
- Backward compatibility

## 🔄 Recommended Workflow

### 1. Initialization

```bash
cotscli database init --db-file simulation.db
```
