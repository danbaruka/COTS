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
