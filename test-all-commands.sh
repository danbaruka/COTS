#!/bin/bash

# COTS Complete Command Test Suite
# Test all implemented commands and features

set -e

echo "🧪 COTS Complete Command Test Suite"
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
    echo -e "${BLUE}[TEST]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

print_error() {
    echo -e "${RED}[FAIL]${NC} $1"
}

# Test counter
TESTS_PASSED=0
TESTS_FAILED=0

# Function to run test
run_test() {
    local test_name="$1"
    local command="$2"
    
    print_status "Running: $test_name"
    echo "Command: $command"
    
    if eval "$command" > /dev/null 2>&1; then
        print_success "$test_name"
        ((TESTS_PASSED++))
    else
        print_error "$test_name"
        ((TESTS_FAILED++))
    fi
    echo
}

# Clean up from previous runs
cleanup() {
    print_status "Cleaning up previous test files..."
    rm -f *.vkey *.skey *.addr *.raw *.signed *.json test.db snapshot_*.db
    rm -f test_utxos.json test_config.json
}

# Create test files
create_test_files() {
    print_status "Creating test files..."
    
    # Create test UTXOs
    cat > test_utxos.json << 'EOF'
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
EOF

    # Create test config
    cat > test_config.json << 'EOF'
{
  "network": "testnet",
  "protocolParameters": {
    "minFeeA": 44,
    "minFeeB": 155381,
    "maxTxSize": 16384,
    "maxValSize": 5000,
    "minUTxOValue": 1000000,
    "priceMemory": 0.0577,
    "priceSteps": 0.0000721,
    "maxTxExUnits": {
      "memory": 14000000,
      "steps": 10000000000
    },
    "maxBlockExUnits": {
      "memory": 62000000,
      "steps": 20000000000
    },
    "maxValueSize": 5000,
    "collateralPercentage": 150,
    "maxCollateralInputs": 3
  }
}
EOF

    print_success "Test files created"
    echo
}

# Test 1: Version command
test_version() {
    run_test "Version command" "cotscli version"
}

# Test 2: Help command
test_help() {
    run_test "Help command" "cotscli --help"
}

# Test 3: Database commands
test_database_commands() {
    print_status "Testing database commands..."
    
    run_test "Database init" "cotscli database init --db-file test.db"
    run_test "Database import UTXOs" "cotscli database import-utxo --db-file test.db --utxo-file test_utxos.json"
    run_test "Database export UTXOs" "cotscli database export-utxo --db-file test.db --out-file exported_utxos.json"
    run_test "Database snapshot" "cotscli database snapshot --db-file test.db --out-file snapshot1.db"
    run_test "Database inspect" "cotscli database inspect --db-file test.db"
    run_test "Database reset" "cotscli database reset --db-file test.db"
}

# Test 4: Address commands
test_address_commands() {
    print_status "Testing address commands..."
    
    run_test "Address key-gen" "cotscli address key-gen --verification-key-file test_payment.vkey --signing-key-file test_payment.skey"
    run_test "Address build" "cotscli address build --payment-verification-key-file test_payment.vkey --out-file test_address.addr --testnet-magic 1097911063"
    run_test "Address info" "cotscli address info --address addr_test1vq0puw93hssk4h432nxyv9x4c25e3x0jemnj8efdqljzwnmc5qj8"
}

# Test 5: Stake address commands
test_stake_address_commands() {
    print_status "Testing stake address commands..."
    
    run_test "Stake address key-gen" "cotscli stake-address key-gen --verification-key-file test_stake.vkey --signing-key-file test_stake.skey"
    run_test "Stake address build" "cotscli stake-address build --stake-verification-key-file test_stake.vkey --out-file test_stake.addr --testnet-magic 1097911063"
    run_test "Stake address info" "cotscli stake-address info --address stake_test1uqpuw93hssk4h432nxyv9x4c25e3x0jemnj8efdqljzwnmc5qj8"
}

# Test 6: Minting commands
test_minting_commands() {
    print_status "Testing minting commands..."
    
    run_test "Mint calculate" "cotscli mint calculate --policy-id policy123 --asset-name token789 --quantity 1000 --protocol-params-file test_config.json"
    run_test "Mint build" "cotscli mint build --tx-in '1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef#0' --tx-out 'addr_test1vq0puw93hssk4h432nxyv9x4c25e3x0jemnj8efdqljzwnmc5qj8 + 1000000 + 1000 policy123.token789' --mint '1000 policy123.token789' --out-file test_mint.raw --testnet-magic 1097911063 --protocol-params-file test_config.json"
}

# Test 7: Transaction commands
test_transaction_commands() {
    print_status "Testing transaction commands..."
    
    run_test "Transaction build" "cotscli transaction build --tx-in '1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef#0' --tx-out 'addr_test1vq0puw93hssk4h432nxyv9x4c25e3x0jemnj8efdqljzwnmc5qj8 + 1000000' --out-file test_tx.raw --testnet-magic 1097911063 --protocol-params-file test_config.json"
    run_test "Transaction simulate" "cotscli transaction simulate --tx-file test_tx.raw"
    run_test "Transaction sign" "cotscli transaction sign --tx-body-file test_tx.raw --signing-key-file test_payment.skey --out-file test_tx.signed"
    run_test "Transaction validate" "cotscli transaction validate --tx-file test_tx.signed"
    run_test "Transaction export" "cotscli transaction export --tx-file test_tx.signed --out-file test_tx.json"
    run_test "Transaction decode" "cotscli transaction decode --tx-file test_tx.signed"
}

# Test 8: UTXO commands
test_utxo_commands() {
    print_status "Testing UTXO commands..."
    
    run_test "UTXO list" "cotscli utxo list --utxo-file test_utxos.json"
    run_test "UTXO list verbose" "cotscli utxo list --utxo-file test_utxos.json --verbose"
    run_test "UTXO reserve" "cotscli utxo reserve --address addr_test1vq0puw93hssk4h432nxyv9x4c25e3x0jemnj8efdqljzwnmc5qj8 --amount 1000000 --utxo-file test_utxos.json --out-file reserved_utxos.json"
}

# Test 9: Protocol commands
test_protocol_commands() {
    print_status "Testing protocol commands..."
    
    run_test "Protocol update" "cotscli protocol update --protocol-params-file test_config.json --out-file updated_params.json"
}

# Test 10: Database advanced features
test_database_advanced() {
    print_status "Testing database advanced features..."
    
    # Reinitialize database
    cotscli database init --db-file test.db
    cotscli database import-utxo --db-file test.db --utxo-file test_utxos.json
    
    run_test "Database load snapshot" "cotscli database load-snapshot --snapshot-file snapshot1.db --db-file test_restored.db"
    run_test "Database export after operations" "cotscli database export-utxo --db-file test.db --out-file final_utxos.json"
}

# Test 11: Error handling
test_error_handling() {
    print_status "Testing error handling..."
    
    # Test with non-existent files
    if ! cotscli database import-utxo --db-file test.db --utxo-file nonexistent.json 2>/dev/null; then
        print_success "Error handling: non-existent file"
        ((TESTS_PASSED++))
    else
        print_error "Error handling: non-existent file"
        ((TESTS_FAILED++))
    fi
    
    # Test with invalid address
    if ! cotscli address info --address invalid_address 2>/dev/null; then
        print_success "Error handling: invalid address"
        ((TESTS_PASSED++))
    else
        print_error "Error handling: invalid address"
        ((TESTS_FAILED++))
    fi
    
    echo
}

# Test 12: File generation verification
test_file_generation() {
    print_status "Testing file generation..."
    
    local files_created=0
    local expected_files=("test_payment.vkey" "test_payment.skey" "test_stake.vkey" "test_stake.skey" 
                         "test_address.addr" "test_stake.addr" "test_tx.raw" "test_tx.signed" 
                         "test_mint.raw" "test.db" "snapshot1.db")
    
    for file in "${expected_files[@]}"; do
        if [[ -f "$file" ]]; then
            print_success "File generated: $file"
            ((files_created++))
        else
            print_warning "File not found: $file"
        fi
    done
    
    if [[ $files_created -gt 0 ]]; then
        print_success "File generation test: $files_created files created"
        ((TESTS_PASSED++))
    else
        print_error "File generation test: no files created"
        ((TESTS_FAILED++))
    fi
    
    echo
}

# Test 13: Command help
test_command_help() {
    print_status "Testing command help..."
    
    run_test "Transaction help" "cotscli transaction --help"
    run_test "Address help" "cotscli address --help"
    run_test "Stake address help" "cotscli stake-address --help"
    run_test "Mint help" "cotscli mint --help"
    run_test "UTXO help" "cotscli utxo --help"
    run_test "Database help" "cotscli database --help"
    run_test "Protocol help" "cotscli protocol --help"
}

# Test 14: Network options
test_network_options() {
    print_status "Testing network options..."
    
    run_test "Mainnet option" "cotscli address build --payment-verification-key-file test_payment.vkey --out-file mainnet_addr.addr --mainnet"
    run_test "Testnet option" "cotscli address build --payment-verification-key-file test_payment.vkey --out-file testnet_addr.addr --testnet-magic 1097911063"
}

# Test 15: Verbose output
test_verbose_output() {
    print_status "Testing verbose output..."
    
    run_test "UTXO list verbose" "cotscli utxo list --utxo-file test_utxos.json --verbose"
    run_test "Database inspect verbose" "cotscli database inspect --db-file test.db"
}

# Main test function
run_all_tests() {
    print_status "Starting comprehensive COTS command test suite..."
    echo
    
    cleanup
    create_test_files
    
    test_version
    test_help
    test_database_commands
    test_address_commands
    test_stake_address_commands
    test_minting_commands
    test_transaction_commands
    test_utxo_commands
    test_protocol_commands
    test_database_advanced
    test_error_handling
    test_file_generation
    test_command_help
    test_network_options
    test_verbose_output
    
    # Print summary
    echo "==================================="
    echo "🧪 TEST SUMMARY"
    echo "==================================="
    echo "Tests passed: $TESTS_PASSED"
    echo "Tests failed: $TESTS_FAILED"
    echo "Total tests: $((TESTS_PASSED + TESTS_FAILED))"
    
    if [[ $TESTS_FAILED -eq 0 ]]; then
        print_success "All tests passed! 🎉"
        exit 0
    else
        print_error "Some tests failed! ❌"
        exit 1
    fi
}

# Check if cotscli is available
check_prerequisites() {
    if ! command -v cotscli &> /dev/null; then
        print_error "cotscli not found. Please build the project first."
        print_status "Run: cabal build"
        exit 1
    fi
    
    print_success "cotscli found and ready for testing"
    echo
}

# Main execution
main() {
    check_prerequisites
    run_all_tests
}

# Run main function
main "$@" 