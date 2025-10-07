#!/bin/bash

# Test runner for syn

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

SYN="./target/debug/syn"
FIXTURES_DIR="./packages/syn/tests/fixtures"

echo "Running syn tests..."
echo

passed=0
failed=0

# Test each fixture
for fixture in $FIXTURES_DIR/*.ml; do
    base=$(basename "$fixture")
    
    # Test token-stream
    echo -n "Testing token-stream for $base... "
    expected="$fixture.token-stream.expected"
    if [ -f "$expected" ]; then
        output=$($SYN token-stream "$fixture" 2>&1)
        expected_content=$(cat "$expected")
        if [ "$output" = "$expected_content" ]; then
            echo -e "${GREEN}PASSED${NC}"
            ((passed++))
        else
            echo -e "${RED}FAILED${NC}"
            echo "  Expected output in: $expected"
            ((failed++))
        fi
    else
        echo -e "${RED}FAILED${NC} (no expected file)"
        ((failed++))
    fi
    
    # Test parse
    echo -n "Testing parse for $base... "
    expected="$fixture.parse.expected"
    if [ -f "$expected" ]; then
        output=$($SYN parse --json "$fixture" 2>&1 | jq '.')
        expected_content=$(cat "$expected")
        if [ "$output" = "$expected_content" ]; then
            echo -e "${GREEN}PASSED${NC}"
            ((passed++))
        else
            echo -e "${RED}FAILED${NC}"
            echo "  Expected output in: $expected"
            echo "  Actual output:"
            echo "$output" | head -20
            ((failed++))
        fi
    else
        echo -e "${RED}FAILED${NC} (no expected file)"
        ((failed++))
    fi
done

echo
echo "Results: $passed passed, $failed failed"

if [ $failed -ne 0 ]; then
    exit 1
fi