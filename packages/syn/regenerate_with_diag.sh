#!/bin/bash

UPDATED=0
SKIPPED=0

for ml_file in tests/fixtures/*.ml; do
    expected_file="${ml_file}.expected"
    
    [[ ! -f "$ml_file" ]] && continue
    [[ ! -f "$expected_file" ]] && continue
    
    # Parse the file
    actual_result=$(../../target/debug/out/syn/syn parse "$ml_file" --json 2>&1 | tail -1)
    
    # Get actual diagnostic count
    actual_diag=$(echo "$actual_result" | jq '.diagnostics | length' 2>/dev/null)
    [[ ! "$actual_diag" =~ ^[0-9]+$ ]] && continue
    
    # Get expected diagnostic count
    expected_diag=$(cat "$expected_file" | jq '.diagnostics | length' 2>/dev/null)
    [[ ! "$expected_diag" =~ ^[0-9]+$ ]] && continue
    
    # If diagnostic counts match but trees differ, update
    if [[ "$actual_diag" == "$expected_diag" ]]; then
        actual_tree=$(echo "$actual_result" | jq -c '.tree')
        expected_tree=$(cat "$expected_file" | jq -c '.tree')
        
        if [[ "$actual_tree" != "$expected_tree" ]]; then
            echo "$actual_result" > "$expected_file"
            UPDATED=$((UPDATED + 1))
            echo "↻ $(basename $ml_file) - same diag count ($actual_diag), different tree"
        fi
    fi
done

echo ""
echo "Summary:"
echo "  Updated: $UPDATED"
