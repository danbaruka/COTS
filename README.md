# Cardano Offline Transaction Simulator (COTS)

COTS est un outil CLI pour simuler des transactions Cardano hors-ligne, calculer les frais, gérer les UTXOs, et exporter les transactions au format Cardano CLI ou Koios API.

## Installation

```bash
stack build
```

## Utilisation de base

### Simuler une transaction

```bash
cotscli simulate -c examples/config.json -f alice -t <adresse_dest> -a 100000000 -v
```

### Exporter une transaction

```bash
cotscli simulate -c examples/config.json -f alice -t <adresse_dest> -a 100000000 --export-cardano-cli
cotscli simulate -c examples/config.json -f alice -t <adresse_dest> -a 100000000 --export-koios
```

### Valider une configuration ou une transaction

```bash
cotscli validate -c examples/config.json
```

### Simuler un script Plutus

```bash
cotscli simulate -c examples/config.json -f alice -t <adresse_dest> -a 100000000 --script plutus.plutus --datum datum.json --redeemer redeemer.json
```

## Exemples de fichiers de configuration

Voir `examples/config.json` et `examples/config.yaml` pour la structure attendue.

## Commandes disponibles

- `simulate` : Simuler une transaction Cardano
- `validate` : Valider une configuration ou une transaction
- `export` : Exporter une transaction (Cardano CLI, Koios)
- `update-protocol` : Mettre à jour les paramètres de protocole
- `version` : Afficher la version

## Options principales

- `-c, --config FILE` : Fichier de configuration (JSON ou YAML)
- `-f, --from WALLET` : Nom du wallet source
- `-t, --to ADDRESS` : Adresse de destination
- `-a, --amount` : Montant en lovelace
- `-s, --script` : Fichier script Plutus
- `--datum` : Fichier datum JSON
- `--redeemer` : Fichier redeemer JSON
- `--export-cardano-cli` : Exporter au format Cardano CLI
- `--export-koios` : Exporter au format Koios API
- `-o, --output` : Fichier de sortie
- `-v, --verbose` : Affichage détaillé

## Tests

Pour lancer les tests unitaires et d'intégration :

```bash
stack test
```

## Licence

MIT

## 🎮 Commandes cardano-cli compatibles

COTS implémente les mêmes commandes que `cardano-cli` pour une expérience familière :

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

COTS utilise SQLite pour gérer l'état UTxO de manière persistante et robuste. Cela permet de :

- **Persistance entre sessions** : Garder trace des UTxOs, balances, paramètres protocole
- **Simulation précise** : Mimer l'évolution d'état d'un vrai nœud Cardano
- **Contrôle développeur** : Snapshots, imports/exports, reset
- **Portabilité** : Base de données unique, facile à versionner et déboguer

### Commandes de base de données

```bash
# Initialiser une nouvelle base de données
cotscli database init --db-file cots.db

# Réinitialiser la base (supprime toutes les tables)
cotscli database reset --db-file cots.db

# Créer un snapshot de l'état actuel
cotscli database snapshot --db-file cots.db --out-file snapshot.db

# Charger un snapshot
cotscli database load-snapshot --snapshot-file snapshot.db --db-file cots.db

# Importer des UTxOs depuis un fichier JSON
cotscli database import-utxo --db-file cots.db --utxo-file utxos.json

# Exporter les UTxOs vers un fichier JSON
cotscli database export-utxo --db-file cots.db --out-file exported_utxos.json

# Inspecter les statistiques de la base
cotscli database inspect --db-file cots.db
```

### Structure de la base de données

La base SQLite contient les tables suivantes :

- **`utxos`** : État actuel des UTxOs par adresse
- **`tx_history`** : Historique des transactions simulées
- **`wallets`** : Adresses et métadonnées des wallets
- **`protocol_params`** : Paramètres protocole utilisés
- **`script_logs`** : Résultats d'exécution des scripts Plutus
- **`assets`** : Métadonnées des tokens/tokens suivis

### Workflow recommandé

1. **Initialiser** : `cotscli database init`
2. **Importer des UTxOs** : `cotscli database import-utxo --utxo-file initial_utxos.json`
3. **Simuler des transactions** : Les UTxOs sont automatiquement mis à jour
4. **Créer des snapshots** : `cotscli database snapshot` pour sauvegarder l'état
5. **Exporter les résultats** : `cotscli database export-utxo` pour partager

### Avantages vs JSON

| Fonctionnalité        | SQLite                        | JSON                          |
| --------------------- | ----------------------------- | ----------------------------- |
| Intégrité des données | ✅ ACID-safe                  | ❌ Corruption possible        |
| Performance           | ✅ Lectures/écritures rapides | ❌ Plus lent avec gros état   |
| Requêtes avancées     | ✅ SQL complet                | ❌ Chargement/filtrage manuel |
| Multi-tables          | ✅ Facile                     | ❌ Nesting manuel             |
| Portabilité           | ✅ Fichier unique             | ✅ Fichier unique             |

---

## 🆕 Nouvelles fonctionnalités

### 🔧 Fee Calculator amélioré

- Calcul précis des frais selon les règles Cardano
- Validation min-UTxO automatique
- Support multi-assets
- Calcul des frais de script Plutus

### 🪙 Minting & Burning

- Simulation complète du minting de tokens natifs
- Support des politiques de minting (native et Plutus)
- Validation des métadonnées d'assets
- Calcul des frais de minting

### 🔑 HD Wallet (CIP-1852)

- Dérivation d'adresses selon CIP-1852
- Rotation automatique des adresses de change
- Support des phrases mnémoniques BIP-39
- Gestion des clés de paiement et de staking

### 📊 Snapshots avancés

- Branchement d'états pour les expériences
- Différenciation entre snapshots
- Gestion des versions d'état
- Export/import de configurations complètes

## 🚀 Workflow de développement

### 1. Setup initial

```bash
# Initialiser la base de données
cotscli database init --db-file dev.db

# Générer les clés
cotscli address key-gen --verification-key-file payment.vkey --signing-key-file payment.skey
cotscli stake-address key-gen --verification-key-file stake.vkey --signing-key-file stake.skey

# Construire les adresses
cotscli address build --payment-verification-key-file payment.vkey --stake-verification-key-file stake.vkey --out-file address.addr
cotscli stake-address build --stake-verification-key-file stake.vkey --out-file stake.addr
```

### 2. Import état initial

```bash
# Importer les UTxOs de départ
cotscli database import-utxo --db-file dev.db --utxo-file initial_utxos.json

# Créer un snapshot de l'état initial
cotscli database snapshot --db-file dev.db --out-file initial_state.db
```

### 3. Développement et tests

```bash
# Simuler des transactions
cotscli transaction build --tx-in <input> --tx-out <output> --out-file tx.raw
cotscli transaction simulate --tx-file tx.raw

# Tester le minting
cotscli mint build --tx-in <input> --mint "1000 <policy>.<asset>" --out-file mint.raw
cotscli mint calculate --policy-id <policy> --asset-name <asset> --quantity 1000

# Créer des snapshots intermédiaires
cotscli database snapshot --db-file dev.db --out-file step1.db
```

### 4. Validation et export

```bash
# Inspecter l'état final
cotscli database inspect --db-file dev.db

# Exporter les résultats
cotscli database export-utxo --db-file dev.db --out-file final_utxos.json
cotscli transaction export --tx-file final.signed --out-file final.json
```

## 🎯 Avantages vs cardano-cli

| Fonctionnalité    | cardano-cli                 | COTS                            |
| ----------------- | --------------------------- | ------------------------------- |
| **Mode offline**  | ❌ Nécessite un nœud        | ✅ Complètement offline         |
| **Simulation**    | ❌ Pas de simulation        | ✅ Simulation complète          |
| **Snapshots**     | ❌ Pas de gestion d'état    | ✅ Snapshots et rollback        |
| **Développement** | ❌ Difficile pour les tests | ✅ Environnement contrôlé       |
| **Performance**   | ❌ Lent avec gros volumes   | ✅ Rapide et optimisé           |
| **Débogage**      | ❌ Logs limités             | ✅ Logs détaillés et inspection |

## 🔮 Roadmap

### Phase 1 - MVP (✅ Complété)

- [x] Commandes cardano-cli compatibles
- [x] Base de données SQLite
- [x] Fee calculator précis
- [x] Minting simulation
- [x] HD wallet support

### Phase 2 - Developer Delight

- [ ] Plugin system
- [ ] Unit-test harness
- [ ] Typed client library (TypeScript)
- [ ] Epoch clock + slot math
- [ ] Reservation API avec TTL

### Phase 3 - Stretch Goals

- [ ] Ogmios live replay
- [ ] Prometheus metrics
- [ ] gRest stub server
- [ ] Property-based tests
- [ ] Single binary distribution

---

## 🚀 Release & Versioning (Automatique)

Ce projet utilise [release-please](https://github.com/googleapis/release-please) pour automatiser :

- L’incrémentation de version dans le fichier `.cabal`
- La génération du changelog
- La création de tags et de releases GitHub

### Convention de commit

Pour déclencher une release automatiquement, utilise des messages de commit conventionnels :

- `feat: nouvelle fonctionnalité`
- `fix: correction de bug`
- `chore: tâches diverses`
- `docs: documentation`

Exemple :

```sh
git commit -m "feat: prise en charge des tokens multi-assets"
```

### Déclencher une release

1. Pousse tes commits sur la branche `main` :
   ```sh
   git push origin main
   ```
2. Le workflow GitHub Actions s’exécutera automatiquement :
   - Il mettra à jour la version dans le `.cabal` et le `CHANGELOG.md`
   - Il créera un tag et une release GitHub

### Voir les releases

- Rendez-vous dans l’onglet [Releases](https://github.com/danbaruka/COTS/releases) du repo GitHub.
- Le changelog et les artefacts sont générés automatiquement.

---

Pour toute question sur le versioning ou l’automatisation, voir `.github/workflows/release-please.yml` ou demande-moi !
