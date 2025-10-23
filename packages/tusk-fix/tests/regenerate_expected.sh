#!/bin/bash

# Regenerate expected outputs for tusk_fix tests
# Usage: ./regenerate_expected.sh [test_number or pattern]

TUSK_FIX="./target/debug/tusk_fix"
TESTS_DIR="./packages/tusk_fix/tests"

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

# Process each fixture
for fixture in $TESTS_DIR/$PATTERN; do
    # Skip if no files match
    [ -f "$fixture" ] || continue
    
    # Skip .expected files
    if [[ "$fixture" == *.expected ]]; then
        continue
    fi
    
    base=$(basename "$fixture")
    
    echo -n "$base... "
    
    # Run linter and save output
    output=$($TUSK_FIX "$fixture" 2>&1)
    echo "$output" > "$fixture.expected"
    echo "REGENERATED"
    ((regenerated++))
done

echo
echo "Results: $regenerated regenerated"
