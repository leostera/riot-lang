#!/bin/bash

# Script to test syn parser on the entire Riot codebase
# Usage: ./parse_codebase.sh

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

passed=0
failed=0
failed_files=()

echo "Testing syn parser on Riot codebase..."
echo

# Find all .ml and .mli files in packages/
for file in $(find packages/ -name "*.ml" -o -name "*.mli" | sort); do
    # Skip test fixtures
    if [[ "$file" == *"/tests/fixtures/"* ]]; then
        continue
    fi
    
    echo -n "Parsing $file... "
    
    # Run parser and capture output
    output=$(./target/debug/syn parse --json "$file" 2>&1)
    exit_code=$?
    
    if [ $exit_code -ne 0 ]; then
        echo -e "${RED}FAILED${NC} (parser crashed)"
        ((failed++))
        failed_files+=("$file (crashed)")
        continue
    fi
    
    # Check for ERROR or MISSING nodes in parse tree (not just string matches)
    if echo "$output" | grep -q '"kind": "ERROR"' || echo "$output" | grep -q '"kind": "MISSING"'; then
        echo -e "${RED}FAILED${NC} (ERROR/MISSING nodes)"
        ((failed++))
        failed_files+=("$file (parse errors)")
    # Check for diagnostics
    elif echo "$output" | jq -e '.diagnostics | length > 0' > /dev/null 2>&1; then
        diag_count=$(echo "$output" | jq '.diagnostics | length')
        echo -e "${YELLOW}WARNING${NC} (has $diag_count diagnostics)"
        ((failed++))
        failed_files+=("$file ($diag_count diagnostics)")
    else
        echo -e "${GREEN}PASSED${NC}"
        ((passed++))
    fi
done

echo
echo "========================================="
echo "Results: $passed passed, $failed failed"
echo "========================================="

if [ ${#failed_files[@]} -gt 0 ]; then
    echo
    echo "Failed files:"
    for file in "${failed_files[@]}"; do
        echo "  - $file"
    done
fi

if [ $failed -ne 0 ]; then
    exit 1
fi
