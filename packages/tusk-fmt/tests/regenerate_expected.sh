#!/bin/bash

# Regenerate expected outputs for tests that parse successfully
# Usage: ./regenerate_expected.sh [test_number or pattern]

SYN="./target/debug/syn"
FIXTURES_DIR="./packages/syn/tests/fixtures"

# Determine test pattern
if [ -z "$1" ]; then
    PATTERN="*.ml"
    echo "Regenerating all expected outputs..."
else
    PATTERN="*$1*.ml"
    echo "Regenerating expected outputs matching: $1"
fi
echo

regenerated=0
skipped=0

# Process each fixture
for fixture in $FIXTURES_DIR/$PATTERN; do
    # Skip if no files match
    [ -f "$fixture" ] || continue
    base=$(basename "$fixture")
    
    echo -n "$base... "
    
    # Parse and check if successful (no ERROR/MISSING tokens)
    output=$($SYN parse --json "$fixture" 2>&1 | jq '.')
    
    if echo "$output" | grep -q '"ERROR"' || echo "$output" | grep -q '"MISSING"'; then
        echo "SKIPPED (parser has errors)"
        ((skipped++))
    elif echo "$output" | grep -q '"width":0,"children":\[\]'; then
        echo "SKIPPED (empty parse tree)"
        ((skipped++))
    else
        echo "$output" > "$fixture.expected"
        echo "REGENERATED"
        ((regenerated++))
    fi
done

echo
echo "Results: $regenerated regenerated, $skipped skipped"
