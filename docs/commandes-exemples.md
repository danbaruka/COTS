# 🏁 COTS CLI Command Examples — Complete Workflow

## Working Directory and --home Option

By default, all files (db, utxos, keys, etc.) are stored in `~/.COTS_NODE`.
You can change this directory with the global `--home` option:

```bash
cotscli --home /path/to/my_cots_home database init
```

All following commands will use this directory as the root.

---

## 1. Initialize the SQLite Database

```bash
cotscli database init --db-file cots.db
```

## 2. Import Test UTXOs

```bash
cotscli database import-utxo --db-file cots.db --utxo-file utxos.json
```

## 3. Generate a Payment Key Pair

```bash
cotscli address key-gen --verification-key-file payment.vkey --signing-key-file payment.skey
```

## 4. Build an Address from the Public Key

```bash
cotscli address build \
  --payment-verification-key-file payment.vkey \
  --testnet-magic 1097911063 \
  --out-file address.addr
```

## 5. List Available UTXOs

```bash
cotscli utxo list --db-file cots.db
```

## 6. Build a Transaction

```bash
cotscli transaction build \
  --tx-in <TXID>#<TXIX> \
  --tx-out $(cat address.addr)+1000000 \
  --change-address $(cat address.addr) \
  --db-file cots.db \
  --out-file tx.raw
```

## 7. Estimate Transaction Fees

```bash
cotscli transaction estimate-fee \
  --tx-file tx.raw \
  --db-file cots.db
```

## 8. Sign a Transaction (offline)

```bash
cotscli transaction sign \
  --tx-file tx.raw \
  --signing-key-file payment.skey \
  --out-file tx.signed
```

## 9. Simulate a Transaction

```bash
cotscli transaction simulate \
  --tx-file tx.signed \
  --db-file cots.db
```

## 10. Validate a Transaction

```bash
cotscli transaction validate \
  --tx-file tx.signed \
  --db-file cots.db
```

## 11. Export a Transaction (Cardano CLI format)

```bash
cotscli transaction export \
  --tx-file tx.signed \
  --format cardano-cli \
  --out-file tx.exported
```

## 12. Decode a Transaction

```bash
cotscli transaction decode --tx-file tx.signed --verbose
```

## 13. Import a Wallet from a JSON File

```bash
cotscli wallet import \
  --file wallet.json \
  --db-file cots.db
```

## 14. Export a Wallet to a JSON File

```bash
cotscli wallet export \
  --name <WALLET_NAME> \
  --db-file cots.db \
  --file wallet.json
```

## 15. Reserve UTXOs for a Given Amount

```bash
cotscli utxo reserve \
  --address $(cat address.addr) \
  --amount 1000000 \
  --db-file cots.db \
  --out-file reserved-utxos.json
```

## 16. Update Protocol Parameters

```bash
cotscli protocol update \
  --file protocol-params.json \
  --db-file cots.db
```

---

**Notes:**

- Replace `<TXID>`, `<TXIX>`, `<WALLET_NAME>`, etc. with actual values from your environment.
- File paths (`payment.vkey`, `address.addr`, etc.) are relative to your current working directory.
- For more details on each command, use `--help` (e.g., `cotscli transaction build --help`).
