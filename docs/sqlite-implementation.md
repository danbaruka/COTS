# Implémentation SQLite pour COTS

## 🎯 Vue d'ensemble

L'implémentation SQLite pour COTS (Cardano Offline Transaction Simulator) fournit une gestion persistante et robuste de l'état UTxO, permettant des simulations réalistes et contrôlées des transactions Cardano.

## 🏗️ Architecture

### Module Database (`src/COTS/Database.hs`)

Le module principal gère toutes les opérations de base de données :

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

### Structure de données

```haskell
data Database = Database
  { dbConnection :: SQLite.Database
  , dbPath :: FilePath
  }
```

## 📊 Schéma de base de données

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

## 🔧 Fonctionnalités implémentées

### 1. Gestion de base de données

- **Initialisation** : `initDatabase` - Crée les tables et index
- **Fermeture** : `closeDatabase` - Ferme proprement la connexion
- **Reset** : `resetDatabase` - Supprime et recrée toutes les tables

### 2. UTxO Management

- **Import** : `importUTXOs` - Charge des UTxOs depuis JSON
- **Export** : `exportUTXOs` - Sauvegarde les UTxOs en JSON
- **CRUD** : `addUTXO`, `removeUTXO`, `getUTXOs`
- **Balance** : `getWalletBalance` - Calcule le solde d'une adresse

### 3. Snapshots et sauvegarde

- **Snapshot** : `snapshotDatabase` - Crée une copie de la base
- **Load** : `loadSnapshot` - Restaure depuis un snapshot

### 4. Historique et logs

- **Transactions** : `addTransaction`, `getTransactionHistory`
- **Scripts** : `addScriptLog`, `getScriptLogs`
- **Assets** : `addAsset`, `getAssets`

### 5. Inspection

- **Stats** : `inspectDatabase` - Affiche les statistiques

## 🎮 Interface CLI

### Commandes principales

```bash
# Initialisation
cotscli database init --db-file cots.db

# Gestion des UTxOs
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

## 🔒 Sécurité et intégrité

### Transactions ACID

- Toutes les opérations critiques utilisent des transactions SQLite
- Rollback automatique en cas d'erreur
- Cohérence garantie

### Validation des données

- Vérification des contraintes d'unicité
- Validation des formats JSON
- Gestion des erreurs de parsing

### Index de performance

```sql
CREATE INDEX idx_utxos_address ON utxos(address);
CREATE INDEX idx_utxos_tx_hash ON utxos(tx_hash);
CREATE INDEX idx_tx_history_tx_hash ON tx_history(tx_hash);
CREATE INDEX idx_wallets_address ON wallets(address);
```

## 📈 Performance

### Optimisations

- **Index** sur les champs de recherche fréquents
- **Transactions** pour les opérations en lot
- **Préparations** de requêtes pour les opérations répétées
- **JSON** pour les données complexes (assets, metadata)

### Métriques attendues

- **Lecture** : ~1000 UTxOs/seconde
- **Écriture** : ~100 UTxOs/seconde
- **Snapshot** : ~1MB/seconde
- **Recherche** : <10ms par adresse

## 🧪 Tests

### Tests unitaires

- Sérialisation JSON des UTxOs
- Validation des formats de données
- Gestion des erreurs

### Tests d'intégration

- Cycle complet import/export
- Snapshots et restauration
- Performance avec gros volumes

### Tests de régression

- Compatibilité des formats
- Migration de schéma
- Rétrocompatibilité

## 🔄 Workflow recommandé

### 1. Initialisation

```bash
cotscli database init --db-file simulation.db
```

### 2. Import état initial

```bash
cotscli database import-utxo --db-file simulation.db --utxo-file initial.json
```

### 3. Simulation

```bash
# Les transactions mettent à jour automatiquement la base
cotscli transaction simulate --tx-file tx1.json --db-file simulation.db
```

### 4. Snapshots

```bash
cotscli database snapshot --db-file simulation.db --out-file step1.db
```

### 5. Export résultats

```bash
cotscli database export-utxo --db-file simulation.db --out-file final.json
```

## 🚀 Avantages

### ✅ Persistance

- État conservé entre les sessions
- Pas de rechargement nécessaire
- Historique complet

### ✅ Contrôle

- Snapshots pour revenir en arrière
- Reset pour recommencer
- Export/import pour partager

### ✅ Performance

- Requêtes SQL optimisées
- Index sur les champs critiques
- Transactions ACID

### ✅ Débogage

- Fichier unique facile à inspecter
- Historique des opérations
- Logs détaillés

### ✅ Portabilité

- SQLite fonctionne partout
- Fichier unique
- Compatible avec les outils existants

## 🔮 Évolutions futures

### Fonctionnalités prévues

- **Requêtes avancées** : SQL personnalisé
- **Migrations** : Mise à jour de schéma
- **Réplication** : Synchronisation entre bases
- **Chiffrement** : Protection des données sensibles
- **Compression** : Optimisation de l'espace

### Intégrations

- **APIs** : Interface REST pour la base
- **Monitoring** : Métriques en temps réel
- **Backup** : Sauvegarde automatique
- **Clustering** : Distribution des données

## 📚 Ressources

### Documentation

- [README.md](../README.md) - Guide utilisateur
- [examples/database-example.md](../examples/database-example.md) - Exemples d'utilisation
- [examples/sqlite-demo.sh](../examples/sqlite-demo.sh) - Script de démonstration

### Tests

- [test/DatabaseSpec.hs](../test/DatabaseSpec.hs) - Tests unitaires
- [test/Spec.hs](../test/Spec.hs) - Suite de tests complète

### Dépendances

- `sqlite3 >=0.5` - Interface SQLite
- `time >=1.9` - Gestion des timestamps
- `aeson >=2.0` - Sérialisation JSON
