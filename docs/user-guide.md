# COTS User Guide

## Overview

COTS (Cardano Offline Transaction Simulator) is a command-line tool that simulates Cardano transactions offline, providing a cardano-cli compatible interface. It uses SQLite for data persistence and supports UTXO management, wallet operations, and transaction simulation.

## Installation

```bash
# Build the project
stack build

# Install globally
stack install
```

## Configuration

COTS stores all data in `~/.COTS_NODE/` directory:

- Database: `~/.COTS_NODE/cots.db`
- Keys: `~/.COTS_NODE/keys/`
- Addresses: `~/.COTS_NODE/addresses/`
- Transactions: `~/.COTS_NODE/transactions/`

## Database Management

### Initialize Database

Initialize a new SQLite database:

```bash
cotscli database init --db-file cots.db
```

### Inspect Database

View database statistics:

```bash
cotscli database inspect --db-file cots.db
```

Output includes:

- Number of UTXOs (unspent and spent)
- Total lovelace
- Number of transactions
- Number of wallets
- Number of protocol parameters

### Reset Database

⚠️ **Dangerous**: Completely wipes the database:

```bash
cotscli database reset --db-file cots.db
```

### Snapshot Operations

Create a database snapshot:

```bash
cotscli database snapshot --db-file cots.db --out-file snapshot.db
```

Load from snapshot:

```bash
cotscli database load-snapshot --snapshot-file snapshot.db --db-file cots.db
```

### Import/Export UTXOs

Import UTXOs from JSON file:

```bash
cotscli database import-utxo --db-file cots.db --utxo-file utxos.json
```

Export UTXOs to JSON file:

```bash
cotscli database export-utxo --db-file cots.db --out-file utxos.json
```

## Wallet Management

### Create Wallet

Create a new wallet:

```bash
cotscli wallet create --name alice --address addr_test1qalice --db-file cots.db
```

### List Wallets

List all wallets in the database:

```bash
cotscli wallet list --db-file cots.db
```

### Wallet Information

Show detailed information about a wallet:

```bash
cotscli wallet info --name alice --db-file cots.db
```

### Import/Export Wallets

Import wallet from JSON file:

```bash
cotscli wallet import --file wallet.json --db-file cots.db
```

Export wallet to JSON file:

```bash
cotscli wallet export --name alice --file wallet.json --db-file cots.db
```

## UTXO Management

### List UTXOs

List all unspent UTXOs:

```bash
cotscli utxo list --db-file cots.db
```

Filter by address:

```bash
cotscli utxo list --address addr_test1qalice --db-file cots.db
```

### Reserve UTXOs

Reserve UTXOs for a specific amount:

```bash
cotscli utxo reserve --address addr_test1qalice --amount 1000000 --db-file cots.db --out-file reserved.json
```

## Transaction Operations

### Build Transaction

Build a transaction using data from the database:

```bash
cotscli transaction build \
  --tx-in "1234567890abcdef#0" \
  --tx-out "addr_test1qbob+1000000" \
  --change-address addr_test1qalice \
  --db-file cots.db \
  --out-file tx.raw
```

### Simulate Transaction

Simulate a transaction:

```bash
cotscli transaction simulate \
  --tx-file tx.raw \
  --db-file cots.db \
  --verbose
```

### Sign Transaction

Sign a transaction:

```bash
cotscli transaction sign \
  --tx-file tx.raw \
  --signing-key-file alice.skey \
  --out-file tx.signed
```

### Validate Transaction

Validate a transaction:

```bash
cotscli transaction validate \
  --tx-file tx.signed \
  --db-file cots.db
```

### Export Transaction

Export transaction in different formats:

```bash
# Export as Cardano CLI format
cotscli transaction export \
  --tx-file tx.signed \
  --format cardano-cli \
  --out-file tx.json

# Export as Koios format
cotscli transaction export \
  --tx-file tx.signed \
  --format koios \
  --out-file tx.json
```

### Decode Transaction

Decode transaction details:

```bash
cotscli transaction decode \
  --tx-file tx.signed \
  --verbose
```

## Protocol Management

### Update Protocol Parameters

Update protocol parameters in the database:

```bash
cotscli protocol update \
  --protocol-params-file params.json \
  --db-file cots.db
```

## Address Management

### Generate Keys

Generate payment key pair:

```bash
cotscli address key-gen \
  --verification-key-file alice.vkey \
  --signing-key-file alice.skey
```

### Build Address

Build address from verification key:

```bash
cotscli address build \
  --payment-verification-key-file alice.vkey \
  --out-file alice.addr \
  --mainnet
```

### Address Information

Show address details:

```bash
cotscli address info --address addr_test1qalice
```

## Stake Address Management

### Generate Stake Keys

Generate stake key pair:

```bash
cotscli stake-address key-gen \
  --verification-key-file stake.vkey \
  --signing-key-file stake.skey
```

### Build Stake Address

Build stake address:

```bash
cotscli stake-address build \
  --stake-verification-key-file stake.vkey \
  --out-file stake.addr \
  --mainnet
```

### Stake Address Information

Show stake address details:

```bash
cotscli stake-address info --address stake_test1qalice
```

## Minting Operations

### Build Minting Transaction

Build a transaction with minting:

```bash
cotscli mint build \
  --tx-in "1234567890abcdef#0" \
  --tx-out "addr_test1qbob+1000000" \
  --mint "100 policy123.token456" \
  --mint-script-file policy.script \
  --out-file mint.raw \
  --mainnet \
  --protocol-params-file params.json
```

### Calculate Minting Fees

Calculate fees for minting:

```bash
cotscli mint calculate \
  --policy-id policy123 \
  --asset-name token456 \
  --quantity 100 \
  --protocol-params-file params.json
```

## Examples

### Complete Transaction Workflow

1. Initialize database:

```bash
cotscli database init --db-file cots.db
```

2. Create wallets:

```bash
cotscli wallet create --name alice --address addr_test1qalice --db-file cots.db
cotscli wallet create --name bob --address addr_test1qbob --db-file cots.db
```

3. Import UTXOs:

```bash
cotscli database import-utxo --db-file cots.db --utxo-file utxos.json
```

4. Build transaction:

```bash
cotscli transaction build \
  --tx-in "1234567890abcdef#0" \
  --tx-out "addr_test1qbob+1000000" \
  --change-address addr_test1qalice \
  --db-file cots.db \
  --out-file tx.raw
```

5. Sign transaction:

```bash
cotscli transaction sign \
  --tx-file tx.raw \
  --signing-key-file alice.skey \
  --out-file tx.signed
```

6. Validate transaction:

```bash
cotscli transaction validate \
  --tx-file tx.signed \
  --db-file cots.db
```

### Database Backup and Restore

1. Create snapshot:

```bash
cotscli database snapshot --db-file cots.db --out-file backup.db
```

2. Restore from snapshot:

```bash
cotscli database load-snapshot --snapshot-file backup.db --db-file restored.db
```

## Troubleshooting

### Common Issues

1. **Database not found**: Ensure the database file exists and has proper permissions
2. **UTXO not found**: Check that UTXOs have been imported into the database
3. **Wallet not found**: Verify the wallet name exists in the database
4. **Permission errors**: Check file permissions for `~/.COTS_NODE/` directory

### Debug Mode

Enable verbose output for detailed information:

```bash
cotscli utxo list --db-file cots.db --verbose
```

## Version Information

Check COTS version:

```bash
cotscli version
```

## SQLite Integration

COTS uses SQLite for data persistence with the following schema:

- **utxos**: Transaction outputs with amounts and assets
- **transactions**: Transaction metadata and status
- **wallets**: Wallet information and addresses
- **protocol_params**: Protocol parameters with timestamps
- **metadata**: Key-value storage for configuration

All data is stored in `~/.COTS_NODE/cots.db` by default, providing ACID compliance and efficient querying capabilities.
