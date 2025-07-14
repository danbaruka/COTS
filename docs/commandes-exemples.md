# 🏁 Exemples de commandes COTS CLI — Workflow complet

## 1. Initialiser la base de données SQLite

```bash
cotscli database init --db-file cots.db
```

## 2. Importer des UTXOs de test

```bash
cotscli database import-utxo --db-file cots.db --utxo-file utxos.json
```

## 3. Générer une paire de clés de paiement

```bash
cotscli address key-gen --verification-key-file payment.vkey --signing-key-file payment.skey
```

## 4. Construire une adresse à partir de la clé publique

```bash
cotscli address build \
  --payment-verification-key-file payment.vkey \
  --testnet-magic 1097911063 \
  --out-file address.addr
```

## 5. Lister les UTXOs disponibles

```bash
cotscli utxo list --db-file cots.db
```

## 6. Construire une transaction

```bash
cotscli transaction build \
  --tx-in <TXID>#<TXIX> \
  --tx-out $(cat address.addr)+1000000 \
  --change-address $(cat address.addr) \
  --db-file cots.db \
  --out-file tx.raw
```

## 7. Estimer les frais d'une transaction

```bash
cotscli transaction estimate-fee \
  --tx-file tx.raw \
  --db-file cots.db
```

## 8. Signer une transaction (offline)

```bash
cotscli transaction sign \
  --tx-file tx.raw \
  --signing-key-file payment.skey \
  --out-file tx.signed
```

## 9. Simuler une transaction

```bash
cotscli transaction simulate \
  --tx-file tx.signed \
  --db-file cots.db
```

## 10. Valider une transaction

```bash
cotscli transaction validate \
  --tx-file tx.signed \
  --db-file cots.db
```

## 11. Exporter une transaction (format Cardano CLI)

```bash
cotscli transaction export \
  --tx-file tx.signed \
  --format cardano-cli \
  --out-file tx.exported
```

## 12. Décoder une transaction

```bash
cotscli transaction decode --tx-file tx.signed --verbose
```

## 13. Importer un wallet depuis un fichier JSON

```bash
cotscli wallet import \
  --file wallet.json \
  --db-file cots.db
```

## 14. Exporter un wallet vers un fichier JSON

```bash
cotscli wallet export \
  --name <WALLET_NAME> \
  --db-file cots.db \
  --file wallet.json
```

## 15. Réserver des UTXOs pour un montant donné

```bash
cotscli utxo reserve \
  --address $(cat address.addr) \
  --amount 1000000 \
  --db-file cots.db \
  --out-file reserved-utxos.json
```

## 16. Mettre à jour les paramètres protocolaires

```bash
cotscli protocol update \
  --file protocol-params.json \
  --db-file cots.db
```

---

**Remarques :**

- Remplace `<TXID>`, `<TXIX>`, `<WALLET_NAME>`, etc. par les valeurs réelles de ton environnement.
- Les chemins de fichiers (`payment.vkey`, `address.addr`, etc.) sont relatifs à l’endroit où tu exécutes les commandes.
- Pour plus de détails sur chaque commande, utilise `--help` (ex : `cotscli transaction build --help`).
