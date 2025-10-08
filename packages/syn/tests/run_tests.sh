#!/bin/bash

# Test runner for syn
# Usage: ./run_tests.sh [test_number or pattern]
# Examples:
#   ./run_tests.sh           # Run all tests
#   ./run_tests.sh 0801      # Run test 0801
#   ./run_tests.sh 08        # Run all tests starting with 08
#   ./run_tests.sh type      # Run all tests with "type" in name

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

SYN="./target/debug/syn"
FIXTURES_DIR="./packages/syn/tests/fixtures"

# Determine test pattern
if [ -z "$1" ]; then
    PATTERN="*.ml"
    echo "Running all syn tests..."
else
    PATTERN="*$1*.ml"
    echo "Running syn tests matching: $1"
fi
echo

passed=0
failed=0

# Test each fixture
for fixture in $FIXTURES_DIR/$PATTERN; do
    # Skip if no files match
    [ -f "$fixture" ] || continue
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
        # Check if expected file has ERROR or MISSING tokens (test expectations are wrong)
        elif echo "$expected_content" | grep -q '"ERROR"' || echo "$expected_content" | grep -q '"MISSING"'; then
            echo -e "${RED}FAILED${NC} (expected file contains ERROR/MISSING tokens)"
            echo "  File: $fixture"
            ((failed++))
        # Check if output has empty SOURCE_FILE node (not implemented)
        elif echo "$output" | grep -q '"width":0,"children":\[\]'; then
            echo -e "${RED}FAILED${NC} (empty parse tree - feature not implemented)"
            echo "  File: $fixture"
            ((failed++))
        # Check if expected file has empty SOURCE_FILE node (not implemented)
        elif echo "$expected_content" | grep -q '"width":0,"children":\[\]'; then
            echo -e "${RED}FAILED${NC} (expected file has empty parse tree)"
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