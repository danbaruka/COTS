# Guide Utilisateur COTS

## Introduction

COTS (Cardano Offline Transaction Simulator) est un outil CLI qui permet de simuler des transactions Cardano hors chaîne, sans avoir besoin d'un nœud ou d'une connexion réseau.

## Installation

### Prérequis

- GHC 9.4+ et Cabal
- Stack (recommandé)

### Installation

```bash
git clone https://github.com/your-username/cardano-offline-transaction-simulator.git
cd cardano-offline-transaction-simulator
stack build
stack install
```

## Configuration

### Fichier de configuration

COTS utilise un fichier de configuration JSON ou YAML pour définir :

- Le réseau (mainnet, testnet, preview, preprod)
- Les paramètres du protocole
- Les wallets et leurs UTXOs

### Exemple de configuration

```json
{
  "network": "testnet",
  "protocol_parameters": {
    "minFeeA": 44,
    "minFeeB": 155381,
    "maxTxSize": 16384,
    "maxValSize": 5000,
    "keyDeposit": 2000000,
    "poolDeposit": 500000000,
    "coinsPerUtxoSize": 4310,
    "maxCollateralInputs": 3,
    "collateralPercentage": 150,
    "maxExecutionUnitsPerTransaction": {
      "memory": 14000000,
      "steps": 10000000000
    }
  },
  "wallets": [
    {
      "name": "alice",
      "address": "addr_test1vq0nckg3mj5z9q345x7p0fnf5w8u92j9hsgnhq0u0n3uq8gq3p5zq",
      "utxos": [
        {
          "txHash": "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef",
          "txIx": 0,
          "amount": {
            "lovelace": 1000000000,
            "assets": {}
          }
        }
      ]
    }
  ]
}
```

## Utilisation

### Simulation d'une transaction simple

```bash
cots simulate --config config.json --from alice --to addr_test1... --amount 100000000
```

### Simulation avec smart contract Plutus

```bash
cots simulate --config config.json --script validator.plutus --datum datum.json --redeemer redeemer.json
```

### Export pour cardano-cli

```bash
cots simulate --config config.json --from alice --to bob --amount 100000000 --export-cardano-cli
```

### Export pour Koios API

```bash
cots simulate --config config.json --from alice --to bob --amount 100000000 --export-koios
```

### Validation de configuration

```bash
cots validate --config config.json
```

### Export de transaction

```bash
cots export --config config.json --transaction tx.json --format cardano-cli
```

## Commandes disponibles

### simulate

Simule une transaction Cardano.

**Options :**

- `--config FILE` : Fichier de configuration (JSON/YAML)
- `--from WALLET` : Nom du wallet source
- `--to ADDRESS` : Adresse de destination
- `--amount LOVELACE` : Montant en lovelace
- `--script FILE` : Fichier script Plutus
- `--datum FILE` : Fichier datum JSON
- `--redeemer FILE` : Fichier redeemer JSON
- `--export-cardano-cli` : Export vers cardano-cli
- `--export-koios` : Export vers API Koios
- `--output FILE` : Fichier de sortie
- `--verbose` : Sortie verbeuse

### validate

Valide une transaction ou un script.

**Options :**

- `--config FILE` : Fichier de configuration
- `--transaction FILE` : Fichier transaction à valider
- `--script FILE` : Fichier script à valider

### export

Exporte une transaction dans différents formats.

**Options :**

- `--config FILE` : Fichier de configuration
- `--transaction FILE` : Fichier transaction à exporter
- `--format FORMAT` : Format d'export (cardano-cli, koios, json)
- `--output FILE` : Fichier de sortie

### update-protocol

Met à jour les paramètres du protocole.

### version

Affiche la version de COTS.

## Format de sortie

### Simulation Result

```json
{
  "success": true,
  "transaction": {
    "txId": "dummy_tx_id_for_simulation",
    "txInputs": [...],
    "txOutputs": [...],
    "txFee": {"unLovelace": 170000},
    "txScripts": [],
    "txDatums": [],
    "txRedeemers": []
  },
  "feeCalculation": {
    "baseFee": {"unLovelace": 170000},
    "sizeFee": {"unLovelace": 0},
    "scriptFee": {"unLovelace": 0},
    "totalFee": {"unLovelace": 170000}
  },
  "errors": [],
  "finalUTXOs": {...},
  "executionUnits": null
}
```

### Cardano CLI Export

```json
{
  "command": "cardano-cli",
  "arguments": [
    "transaction",
    "submit",
    "--tx-file",
    "transaction.signed",
    "--mainnet"
  ],
  "description": "Cardano CLI command for transaction submission"
}
```

### Koios API Export

```json
{
  "endpoint": "https://api.koios.rest/api/v1/submittx",
  "method": "POST",
  "headers": {
    "Content-Type": "application/json",
    "Accept": "application/json"
  },
  "body": "..."
}
```

## Migration vers testnet/mainnet

1. **Remplacer les UTXOs fictifs** par des UTXOs réels récupérés via `cardano-cli query utxo` ou l'API Koios
2. **Signer la transaction** avec les clés réelles
3. **Soumettre** via `cardano-cli` (nœud local) ou Koios (API)

## Dépannage

### Erreurs courantes

- **"Wallet not found"** : Vérifiez que le nom du wallet existe dans la configuration
- **"Insufficient funds"** : Vérifiez que le wallet a suffisamment de lovelace
- **"Invalid address"** : Vérifiez le format de l'adresse Cardano
- **"Script execution failed"** : Vérifiez que le script Plutus est valide

### Logs et débogage

Utilisez l'option `--verbose` pour obtenir plus d'informations de débogage :

```bash
cots simulate --config config.json --from alice --to bob --amount 100000000 --verbose
```

## Support

Pour obtenir de l'aide :

- [Issues GitHub](https://github.com/your-username/cardano-offline-transaction-simulator/issues)
- [Discussions](https://github.com/your-username/cardano-offline-transaction-simulator/discussions)
