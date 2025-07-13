#!/bin/bash

# COTS SQLite Database Demo
# This script demonstrates the SQLite database functionality

set -e

echo "🗄️  COTS SQLite Database Demo"
echo "=============================="
echo

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Clean up from previous runs
cleanup() {
    print_status "Cleaning up previous demo files..."
    rm -f demo.db snapshot_*.db initial_utxos.json final_utxos.json
}

# Step 1: Initialize database
step1_init() {
    print_status "Step 1: Initializing SQLite database..."
    cotscli database init --db-file demo.db
    print_success "Database initialized successfully"
    echo
}

# Step 2: Create sample UTXOs
step2_create_utxos() {
    print_status "Step 2: Creating sample UTXOs..."
    
    cat > initial_utxos.json << 'EOF'
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
  },
  {
    "txHash": "deadbeef1234567890abcdef1234567890abcdef1234567890abcdef1234567890",
    "txIx": 2,
    "amount": {
      "lovelace": 750000000,
      "assets": [
        {
          "assetId": "policy456.token789",
          "quantity": 50
        },
        {
          "assetId": "policy789.token123",
          "quantity": 25
        }
      ]
    }
  }
]
EOF
    
    print_success "Sample UTXOs created in initial_utxos.json"
    echo
}

# Step 3: Import UTXOs
step3_import_utxos() {
    print_status "Step 3: Importing UTXOs into database..."
    cotscli database import-utxo --db-file demo.db --utxo-file initial_utxos.json
    print_success "UTXOs imported successfully"
    echo
}

# Step 4: Inspect database
step4_inspect() {
    print_status "Step 4: Inspecting database..."
    cotscli database inspect --db-file demo.db
    echo
}

# Step 5: Create snapshot
step5_snapshot() {
    print_status "Step 5: Creating snapshot..."
    cotscli database snapshot --db-file demo.db --out-file snapshot_initial.db
    print_success "Snapshot created: snapshot_initial.db"
    echo
}

# Step 6: Simulate transaction (mock)
step6_simulate() {
    print_status "Step 6: Simulating transaction (mock)..."
    print_warning "This would normally update UTXOs in the database"
    print_warning "For demo purposes, we'll just show the concept"
    echo
}

# Step 7: Export current state
step7_export() {
    print_status "Step 7: Exporting current UTXOs..."
    cotscli database export-utxo --db-file demo.db --out-file final_utxos.json
    print_success "UTXOs exported to final_utxos.json"
    echo
}

# Step 8: Show file differences
step8_compare() {
    print_status "Step 8: Comparing initial vs final state..."
    echo "Initial UTXOs:"
    cat initial_utxos.json | jq '.[0:2]' 2>/dev/null || cat initial_utxos.json
    echo
    echo "Final UTXOs:"
    cat final_utxos.json | jq '.[0:2]' 2>/dev/null || cat final_utxos.json
    echo
}

# Step 9: Load snapshot
step9_load_snapshot() {
    print_status "Step 9: Loading snapshot..."
    cotscli database load-snapshot --snapshot-file snapshot_initial.db --db-file demo_restored.db
    print_success "Snapshot loaded to demo_restored.db"
    echo
}

# Step 10: Reset database
step10_reset() {
    print_status "Step 10: Resetting database..."
    cotscli database reset --db-file demo.db
    print_success "Database reset successfully"
    echo
}

# Main demo function
run_demo() {
    print_status "Starting COTS SQLite Database Demo..."
    echo
    
    cleanup
    step1_init
    step2_create_utxos
    step3_import_utxos
    step4_inspect
    step5_snapshot
    step6_simulate
    step7_export
    step8_compare
    step9_load_snapshot
    step10_reset
    
    print_success "Demo completed successfully!"
    echo
    print_status "Files created:"
    ls -la demo.db snapshot_*.db *.json 2>/dev/null || true
    echo
    print_status "You can now explore the database with:"
    echo "  sqlite3 demo.db '.tables'"
    echo "  sqlite3 demo.db 'SELECT * FROM utxos;'"
}

# Check if cotscli is available
check_prerequisites() {
    if ! command -v cotscli &> /dev/null; then
        print_error "cotscli not found. Please build the project first."
        print_status "Run: cabal build"
        exit 1
    fi
    
    if ! command -v jq &> /dev/null; then
        print_warning "jq not found. JSON output will not be formatted."
    fi
}

# Main execution
main() {
    check_prerequisites
    run_demo
}

# Run main function
main "$@" 