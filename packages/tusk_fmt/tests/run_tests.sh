#!/bin/bash

# Test runner for tusk_fmt
# Usage: ./run_tests.sh [test_number or pattern]
# Examples:
#   ./run_tests.sh           # Run all tests
#   ./run_tests.sh 0801      # Run test 0801
#   ./run_tests.sh 08        # Run all tests starting with 08
#   ./run_tests.sh type      # Run all tests with "type" in name

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

FMT="tusk run tusk_fmt --"
FIXTURES_DIR="./packages/tusk_fmt/tests/fixtures"

# Determine test pattern
if [ -z "$1" ]; then
    PATTERN="*.ml.actual"
    echo "Running all tusk_fmt tests..."
else
    PATTERN="*$1*.ml.actual"
    echo "Running tusk_fmt tests matching: $1"
fi
echo

passed=0
failed=0

# Test each fixture
for fixture in $FIXTURES_DIR/$PATTERN; do
    # Skip if no files match
    [ -f "$fixture" ] || continue
    base=$(basename "$fixture" .actual)
    
    # Get expected file (replace .actual with .expected)
    expected="${fixture%.actual}.expected"
    
    # Test format
    echo -n "$base... "
    if [ -f "$expected" ]; then
        # Run formatter and extract just the formatted code
        # Skip compilation output, header, and footer - extract only between @filename and Summary
        output=$($FMT "$fixture" 2>&1 | grep -A 10000 "^@" | grep -B 10000 "^Summary" | grep -v "^@" | grep -v "^Summary" | head -n -2)
        expected_content=$(cat "$expected")
        
        if [ "$output" = "$expected_content" ]; then
            echo -e "${GREEN}PASSED${NC}"
            ((passed++))
        else
            echo -e "${RED}FAILED${NC}"
            echo "  Expected: $expected"
            echo "  To see diff: diff -u $expected <($FMT $fixture 2>&1 | tail -n +2)"
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
