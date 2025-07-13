#!/bin/bash

# COTS cardano-cli Compatible Demo
# This script demonstrates the cardano-cli compatible commands

set -e

echo "🎮 COTS cardano-cli Compatible Demo"
echo "==================================="
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
    rm -f *.vkey *.skey *.addr *.raw *.signed *.json demo.db
}

# Step 1: Generate keys
step1_generate_keys() {
    print_status "Step 1: Generating key pairs..."
    
    # Generate payment keys
    cotscli address key-gen --verification-key-file payment.vkey --signing-key-file payment.skey
    print_success "Payment keys generated"
    
    # Generate stake keys
    cotscli stake-address key-gen --verification-key-file stake.vkey --signing-key-file stake.skey
    print_success "Stake keys generated"
    
    echo
}

# Step 2: Build addresses
step2_build_addresses() {
    print_status "Step 2: Building addresses..."
    
    # Build payment address
    cotscli address build --payment-verification-key-file payment.vkey --stake-verification-key-file stake.vkey --out-file address.addr --testnet-magic 1097911063
    print_success "Payment address built"
    
    # Build stake address
    cotscli stake-address build --stake-verification-key-file stake.vkey --out-file stake.addr --testnet-magic 1097911063
    print_success "Stake address built"
    
    echo
}

# Step 3: Show address info
step3_address_info() {
    print_status "Step 3: Address information..."
    
    echo "Payment Address:"
    cat address.addr
    echo
    
    echo "Stake Address:"
    cat stake.addr
    echo
    
    # Get address info
    cotscli address info --address $(cat address.addr)
    echo
    
    cotscli stake-address info --address $(cat stake.addr)
    echo
}

# Step 4: Initialize database
step4_init_database() {
    print_status "Step 4: Initializing database..."
    
    cotscli database init --db-file demo.db
    print_success "Database initialized"
    
    echo
}

# Step 5: Create sample UTXOs
step5_create_utxos() {
    print_status "Step 5: Creating sample UTXOs..."
    
    cat > initial_utxos.json << 'EOF'
[
  {
    "txHash": "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef",
    "txIx": 0,
    "amount": {
      "lovelace": 1000000000,
      "assets": []
    }
  },
  {
    "txHash": "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890",
    "txIx": 1,
    "amount": {
      "lovelace": 500000000,
      "assets": [
        {
          "assetId": "policy123.token456",
          "quantity": 100
        }
      ]
    }
  }
]
EOF
    
    cotscli database import-utxo --db-file demo.db --utxo-file initial_utxos.json
    print_success "UTXOs imported"
    
    echo
}

# Step 6: List UTXOs
step6_list_utxos() {
    print_status "Step 6: Listing UTXOs..."
    
    cotscli utxo list --utxo-file initial_utxos.json --verbose
    echo
}

# Step 7: Calculate minting fees
step7_mint_calculate() {
    print_status "Step 7: Calculating minting fees..."
    
    cotscli mint calculate --policy-id policy123 --asset-name token789 --quantity 1000 --protocol-params-file examples/config.json
    echo
}

# Step 8: Build minting transaction
step8_mint_build() {
    print_status "Step 8: Building minting transaction..."
    
    cotscli mint build \
        --tx-in "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef#0" \
        --tx-out "addr_test1... + 1000000 + 1000 policy123.token789" \
        --mint "1000 policy123.token789" \
        --change-address $(cat address.addr) \
        --out-file mint.raw \
        --testnet-magic 1097911063 \
        --protocol-params-file examples/config.json
    
    print_success "Minting transaction built"
    echo
}

# Step 9: Simulate transaction
step9_simulate() {
    print_status "Step 9: Simulating transaction..."
    
    cotscli transaction simulate --tx-file mint.raw
    echo
}

# Step 10: Sign transaction
step10_sign() {
    print_status "Step 10: Signing transaction..."
    
    cotscli transaction sign --tx-body-file mint.raw --signing-key-file payment.skey --out-file mint.signed
    print_success "Transaction signed"
    echo
}

# Step 11: Validate transaction
step11_validate() {
    print_status "Step 11: Validating transaction..."
    
    cotscli transaction validate --tx-file mint.signed
    echo
}

# Step 12: Export transaction
step12_export() {
    print_status "Step 12: Exporting transaction..."
    
    cotscli transaction export --tx-file mint.signed --out-file mint.json
    print_success "Transaction exported to mint.json"
    echo
}

# Step 13: Decode transaction
step13_decode() {
    print_status "Step 13: Decoding transaction..."
    
    cotscli transaction decode --tx-file mint.signed
    echo
}

# Step 14: Database inspection
step14_inspect() {
    print_status "Step 14: Inspecting database..."
    
    cotscli database inspect --db-file demo.db
    echo
}

# Main demo function
run_demo() {
    print_status "Starting COTS cardano-cli Compatible Demo..."
    echo
    
    cleanup
    step1_generate_keys
    step2_build_addresses
    step3_address_info
    step4_init_database
    step5_create_utxos
    step6_list_utxos
    step7_mint_calculate
    step8_mint_build
    step9_simulate
    step10_sign
    step11_validate
    step12_export
    step13_decode
    step14_inspect
    
    print_success "Demo completed successfully!"
    echo
    print_status "Files created:"
    ls -la *.vkey *.skey *.addr *.raw *.signed *.json demo.db 2>/dev/null || true
    echo
    print_status "You can now use these files with real cardano-cli commands!"
}

# Check if cotscli is available
check_prerequisites() {
    if ! command -v cotscli &> /dev/null; then
        print_error "cotscli not found. Please build the project first."
        print_status "Run: cabal build"
        exit 1
    fi
}

# Main execution
main() {
    check_prerequisites
    run_demo
}

# Run main function
main "$@" 