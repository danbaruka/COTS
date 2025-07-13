# Exemple d'utilisation de la base de données SQLite

## 🗄️ Initialisation et configuration

```bash
# 1. Initialiser une nouvelle base de données
cotscli database init --db-file cots.db

# 2. Vérifier que la base est créée
ls -la cots.db
```

## 📥 Import d'UTxOs

Créer un fichier `initial_utxos.json` :

```json
[
  {
    "txHash": "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef",
    "txIx": 0,
    "amount": {
      "lovelace": 1000000000,
      "assets": [
        {
          "assetId": "policy123.token456",
          "quantity": 100
        }
      ]
    }
  },
  {
    "txHash": "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890",
    "txIx": 1,
    "amount": {
      "lovelace": 500000000,
      "assets": []
    }
  }
]
```

Importer les UTxOs :

```bash
cotscli database import-utxo --db-file cots.db --utxo-file initial_utxos.json
```

## 🔍 Inspection de la base

```bash
# Voir les statistiques de la base
cotscli database inspect --db-file cots.db

# Sortie attendue :
# Database Statistics:
# ===================
# UTXOs: 2
# Transactions: 0
# Wallets: 0
# Script executions: 0
# Assets: 0
```

## 📸 Snapshots et sauvegarde

```bash
# Créer un snapshot de l'état actuel
cotscli database snapshot --db-file cots.db --out-file before_simulation.db

# Simuler des transactions (les UTxOs sont mis à jour automatiquement)
cotscli transaction simulate --tx-file transaction.json --db-file cots.db

# Créer un autre snapshot après simulation
cotscli database snapshot --db-file cots.db --out-file after_simulation.db
```

## 📤 Export des résultats

```bash
# Exporter les UTxOs actuels
cotscli database export-utxo --db-file cots.db --out-file final_utxos.json

# Le fichier contiendra l'état final après simulation
```

## 🔄 Gestion des états

```bash
# Réinitialiser complètement la base
cotscli database reset --db-file cots.db

# Charger un snapshot précédent
cotscli database load-snapshot --snapshot-file before_simulation.db --db-file cots.db
```

## 🏗️ Workflow complet

```bash
#!/bin/bash

# 1. Initialiser
cotscli database init --db-file simulation.db

# 2. Importer UTxOs initiaux
cotscli database import-utxo --db-file simulation.db --utxo-file initial_state.json

# 3. Créer snapshot initial
cotscli database snapshot --db-file simulation.db --out-file step1_initial.db

# 4. Simuler transaction 1
cotscli transaction simulate --tx-file tx1.json --db-file simulation.db

# 5. Créer snapshot après tx1
cotscli database snapshot --db-file simulation.db --out-file step2_after_tx1.db

# 6. Simuler transaction 2
cotscli transaction simulate --tx-file tx2.json --db-file simulation.db

# 7. Exporter état final
cotscli database export-utxo --db-file simulation.db --out-file final_state.json

# 8. Inspecter les statistiques
cotscli database inspect --db-file simulation.db
```

## 🎯 Avantages de cette approche

### ✅ Persistance

- L'état est conservé entre les sessions
- Pas besoin de recharger les UTxOs à chaque fois

### ✅ Contrôle

- Snapshots pour revenir en arrière
- Reset pour recommencer proprement
- Export/import pour partager des états

### ✅ Performance

- Requêtes SQL rapides
- Index sur les champs importants
- Transactions ACID pour la cohérence

### ✅ Débogage

- Fichier unique facile à inspecter
- Historique des transactions
- Logs d'exécution des scripts

### ✅ Portabilité

- Base SQLite fonctionne partout
- Fichier unique facile à versionner
- Compatible avec les outils existants
