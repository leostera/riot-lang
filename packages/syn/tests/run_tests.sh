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
    
    # Test parse
    echo -n "$base... "
    expected="$fixture.expected"
    if [ -f "$expected" ]; then
        output=$($SYN parse --json "$fixture" 2>&1 | jq '.')
        expected_content=$(cat "$expected")
        
        # Check if output has ERROR or MISSING tokens (parser is broken)
        if echo "$output" | grep -q '"ERROR"' || echo "$output" | grep -q '"MISSING"'; then
            echo -e "${RED}FAILED${NC} (parser produces ERROR/MISSING tokens)"
            echo "  File: $fixture"
            ((failed++))
        elif [ "$output" = "$expected_content" ]; then
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