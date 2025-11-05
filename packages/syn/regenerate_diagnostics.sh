#!/bin/bash

UPDATED=0

for ml_file in tests/diagnostics/*.ml; do
    [[ ! -f "$ml_file" ]] && continue
    
    diag_file="${ml_file}.diagnostic"
    [[ ! -f "$diag_file" ]] && continue
    
    # Parse and extract diagnostics
    actual=$(../../target/debug/out/syn/syn parse "$ml_file" --json 2>&1 | tail -1 | jq '.diagnostics')
    
    # Check if valid JSON
    if ! echo "$actual" | jq '.' > /dev/null 2>&1; then
        echo "✗ $(basename $ml_file) - invalid JSON output"
        continue
    fi
    
    # Compare with expected
    expected=$(cat "$diag_file")
    
    if [[ "$actual" != "$expected" ]]; then
        echo "$actual" > "$diag_file"
        UPDATED=$((UPDATED + 1))
        echo "↻ $(basename $ml_file)"
    fi
done

echo ""
echo "Updated: $UPDATED diagnostic files"
