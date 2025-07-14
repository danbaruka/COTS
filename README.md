# Cardano Offline Transaction Simulator (COTS)

COTS is a CLI tool for simulating Cardano transactions offline, calculating fees, managing UTXOs, and exporting transactions in Cardano CLI or Koios API formats.

## Installation

```bash
stack build
```

## Basic Usage

### Simulate a Transaction

```bash
cotscli simulate -c examples/config.json -f alice -t <destination_address> -a 100000000 -v
```

### Export a Transaction

```bash
cotscli simulate -c examples/config.json -f alice -t <destination_address> -a 100000000 --export-cardano-cli
cotscli simulate -c examples/config.json -f alice -t <destination_address> -a 100000000 --export-koios
```

### Validate a Configuration or Transaction

```bash
cotscli validate -c examples/config.json
```

### Simulate a Plutus Script

```bash
cotscli simulate -c examples/config.json -f alice -t <destination_address> -a 100000000 --script plutus.plutus --datum datum.json --redeemer redeemer.json
```

## Example Configuration Files

See `examples/config.json` and `examples/config.yaml` for the expected structure.

## Available Commands

- `simulate`: Simulate a Cardano transaction
- `validate`: Validate a configuration or transaction
- `export`: Export a transaction (Cardano CLI, Koios)
- `update-protocol`: Update protocol parameters
- `version`: Show version

## Main Options

- `-c, --config FILE`: Configuration file (JSON or YAML)
- `-f, --from WALLET`: Source wallet name
- `-t, --to ADDRESS`: Destination address
- `-a, --amount`: Amount in lovelace
- `-s, --script`: Plutus script file
- `--datum`: Datum JSON file
- `--redeemer`: Redeemer JSON file
- `--export-cardano-cli`: Export in Cardano CLI format
- `--export-koios`: Export in Koios API format
- `-o, --output`: Output file
- `-v, --verbose`: Verbose output

## Testing

To run unit and integration tests:

```bash
stack test
```

## License

MIT

## 🎮 Cardano-CLI Compatible Commands

COTS implements the same commands as `cardano-cli` for a familiar experience:

### Transaction Commands

```bash
# Build transaction (offline simulation)
cotscli transaction build --tx-in <input> --tx-out <output> --out-file tx.raw

# Simulate transaction
cotscli transaction simulate --tx-file tx.raw

# Sign transaction
cotscli transaction sign --tx-body-file tx.raw --signing-key-file key.skey --out-file tx.signed

# Validate transaction
cotscli transaction validate --tx-file tx.signed

# Export transaction
cotscli transaction export --tx-file tx.signed --out-file tx.json

# Decode transaction
cotscli transaction decode --tx-file tx.signed
```

### Address Commands

```bash
# Generate payment key pair
cotscli address key-gen --verification-key-file payment.vkey --signing-key-file payment.skey

# Build address
cotscli address build --payment-verification-key-file payment.vkey --out-file address.addr

# Get address info
cotscli address info --address addr_test1...
```

### Stake Address Commands

```bash
# Generate stake key pair
cotscli stake-address key-gen --verification-key-file stake.vkey --signing-key-file stake.skey

# Build stake address
cotscli stake-address build --stake-verification-key-file stake.vkey --out-file stake.addr

# Get stake address info
cotscli stake-address info --address stake_test1...
```

### Minting Commands

```bash
# Build minting transaction
cotscli mint build --tx-in <input> --tx-out <output> --mint <mint-spec> --out-file mint.raw

# Calculate minting fees
cotscli mint calculate --policy-id <policy> --asset-name <name> --quantity <amount>
```

### UTXO Commands

```bash
# List UTXOs
cotscli utxo list --address addr_test1...

# Reserve UTXOs
cotscli utxo reserve --address addr_test1... --amount 1000000 --out-file reserved.json
```

### Protocol Commands

```bash
# Update protocol parameters
cotscli protocol update --protocol-params-file params.json --out-file updated.json
```

## 🗄️ Database Management (SQLite)

COTS uses SQLite to manage the UTxO state persistently and robustly. This allows:

- **Persistence between sessions**: Track UTxOs, balances, protocol parameters
- **Accurate simulation**: Mimic the state evolution of a real Cardano node
- **Developer control**: Snapshots, imports/exports, reset
- **Portability**: Single database file, easy to version and debug

### Database Commands

```bash
# Initialize a new database
cotscli database init --db-file cots.db

# Reset the database (deletes all tables)
cotscli database reset --db-file cots.db

# Create a snapshot of the current state
cotscli database snapshot --db-file cots.db --out-file snapshot.db

# Load a snapshot
cotscli database load-snapshot --snapshot-file snapshot.db --db-file cots.db

# Import UTxOs from a JSON file
cotscli database import-utxo --db-file cots.db --utxo-file utxos.json

# Export UTxOs to a JSON file
cotscli database export-utxo --db-file cots.db --out-file exported_utxos.json

# Inspect database statistics
cotscli database inspect --db-file cots.db
```

### Database Structure

The SQLite database contains the following tables:

- **`utxos`**: Current UTxO state by address
- **`tx_history`**: Simulated transaction history
- **`wallets`**: Wallet addresses and metadata
- **`protocol_params`**: Protocol parameters in use
- **`script_logs`**: Plutus script execution results
- **`assets`**: Token/asset metadata

### Recommended Workflow

1. **Initialize**: `cotscli database init`
2. **Import UTxOs**: `cotscli database import-utxo --utxo-file initial_utxos.json`
3. **Simulate transactions**: UTxOs are automatically updated
4. **Create snapshots**: `cotscli database snapshot` to save state
5. **Export results**: `cotscli database export-utxo` to share

### Advantages vs JSON

| Feature             | SQLite             | JSON                        |
| ------------------- | ------------------ | --------------------------- |
| Data integrity      | ✅ ACID-safe       | ❌ Possible corruption      |
| Performance         | ✅ Fast read/write | ❌ Slower with large state  |
| Advanced queries    | ✅ Full SQL        | ❌ Manual loading/filtering |
| Multi-table support | ✅ Easy            | ❌ Manual nesting           |
| Portability         | ✅ Single file     | ✅ Single file              |

---

## 🆕 New Features

### 🔧 Improved Fee Calculator

- Accurate fee calculation according to Cardano rules
- Automatic min-UTxO validation
- Multi-asset support
- Plutus script fee calculation

### 🪙 Minting & Burning

- Full simulation of native token minting
- Support for minting policies (native and Plutus)
- Asset metadata validation
- Minting fee calculation

### 🔑 HD Wallet (CIP-1852)

- Address derivation according to CIP-1852
- Automatic change address rotation
- BIP-39 mnemonic phrase support
- Payment and staking key management

### 📊 Advanced Snapshots

- Branching states for experiments
- Differentiation between snapshots
- State version management
- Full configuration export/import

## 🚀 Development Workflow

1. Fork or clone the repository
2. Build with `stack build`
3. Run tests with `stack test`
4. Use the CLI as described above

---

For more details, see the documentation in the `docs/` folder and example configs in `examples/`.
