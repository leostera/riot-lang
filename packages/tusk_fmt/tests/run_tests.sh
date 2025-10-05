#!/bin/bash

# Simple test runner for tusk_fmt

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

FORMATTER="./target/debug/tusk_fmt"
FIXTURES_DIR="./packages/tusk_fmt/tests/fixtures"

echo "Running tusk_fmt tests..."
echo

passed=0
failed=0
skipped=0

./tusk build --package tusk_fmt

# Test each fixture file
for fixture in $FIXTURES_DIR/*.ml; do
    # Skip expected files
    if [[ $fixture == *.ml.expected ]]; then
        continue
    fi
    
    base=$(basename "$fixture")
    expected="$fixture.expected"
    
    echo -n "Testing $base... "
    
    # Run formatter
    output=$($FORMATTER "$fixture" 2>&1)
    exit_code=$?
    
    if [ $exit_code -ne 0 ]; then
        echo -e "${RED}FAILED${NC} (formatter error)"
        echo "  Error: $output"
        ((failed++))
        continue
    fi
    
    # If expected file exists, compare
    if [ -f "$expected" ]; then
        expected_content=$(cat "$expected")
        if [ "$output" = "$expected_content" ]; then
            echo -e "${GREEN}PASSED${NC}"
            ((passed++))
        else
            echo -e "${RED}FAILED${NC} (output mismatch)"
            echo "  Expected: $expected"
            echo "  Got output:"
            echo "$output" | head -5
            ((failed++))
        fi
    else
        # No expected file - just show it ran
        echo -e "${YELLOW}RAN${NC} (no expected output to compare)"
        echo "  Output preview:"
        echo "$output" | head -3
        ((skipped++))
    fi
done

echo
echo "Results: $passed passed, $failed failed, $skipped skipped (no expected output)"

if [ $failed -ne 0 ]; then
    exit 1
fi
