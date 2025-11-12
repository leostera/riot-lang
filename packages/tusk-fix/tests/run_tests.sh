#!/bin/bash

# Test runner for tusk_fix
# Usage: ./run_tests.sh [test_number or pattern]
# Examples:
#   ./run_tests.sh           # Run all tests
#   ./run_tests.sh 0001      # Run test 0001
#   ./run_tests.sh nostdlib  # Run all tests with "nostdlib" in name

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

TUSK_FIX="tusk run tusk-fix:tusk-fix --"
TESTS_DIR="./packages/tusk-fix/tests"

# Determine test pattern
if [ -z "$1" ]; then
    PATTERN="*.ml"
    echo "Running all tusk_fix tests..."
else
    PATTERN="*$1*.ml"
    echo "Running tusk_fix tests matching: $1"
fi
echo

passed=0
failed=0

# Test each fixture
for fixture in $TESTS_DIR/$PATTERN; do
    # Skip if no files match
    [ -f "$fixture" ] || continue
    base=$(basename "$fixture" .ml)
    
    # Skip .expected files
    if [[ "$fixture" == *.expected ]]; then
        continue
    fi
    
    # Test lint
    echo -n "$base... "
    expected="$fixture.expected"
    if [ -f "$expected" ]; then
        output=$($TUSK_FIX "$fixture" 2>&1)
        expected_content=$(cat "$expected")
        
        if [ "$output" = "$expected_content" ]; then
            echo -e "${GREEN}PASSED${NC}"
            ((passed++))
        else
            echo -e "${RED}FAILED${NC}"
            echo "  Expected output in: $expected"
            echo "  Diff:"
            diff -u "$expected" <(echo "$output") | head -40
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
