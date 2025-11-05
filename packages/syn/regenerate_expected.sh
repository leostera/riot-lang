#!/bin/bash

PASSED=0
FAILED=0
SKIPPED=0

for ml_file in tests/fixtures/*.ml; do
    expected_file="${ml_file}.expected"
    
    # Skip if not a .ml file
    [[ ! -f "$ml_file" ]] && continue
    
    # Parse the file
    result=$(../../target/debug/out/syn/syn parse "$ml_file" --json 2>&1 | tail -1)
    
    # Check if it has diagnostics
    diag_count=$(echo "$result" | jq '.diagnostics | length' 2>/dev/null)
    
    if [[ "$diag_count" == "0" ]]; then
        # No diagnostics - regenerate expected file
        echo "$result" > "$expected_file"
        PASSED=$((PASSED + 1))
        echo "✓ $(basename $ml_file)"
    elif [[ "$diag_count" =~ ^[0-9]+$ ]]; then
        # Has diagnostics - skip
        SKIPPED=$((SKIPPED + 1))
        # echo "⊘ $(basename $ml_file) - $diag_count diagnostics"
    else
        # Parse error
        FAILED=$((FAILED + 1))
        echo "✗ $(basename $ml_file) - parse failed"
    fi
done

echo ""
echo "Summary:"
echo "  Regenerated: $PASSED"
echo "  Skipped (has diagnostics): $SKIPPED"
echo "  Failed: $FAILED"
