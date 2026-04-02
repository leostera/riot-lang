#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
WORKSPACE_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

find_syn_bin() {
    local direct
    direct=$(find "$WORKSPACE_ROOT/_build/debug" -path '*/out/syn/syn' -type f 2>/dev/null | sort | head -n 1 || true)
    if [[ -n "$direct" ]]; then
        echo "$direct"
        return 0
    fi

    local fallback
    fallback=$(find "$WORKSPACE_ROOT/_build" -path '*/out/syn/syn' -type f 2>/dev/null | sort | head -n 1 || true)
    if [[ -n "$fallback" ]]; then
        echo "$fallback"
        return 0
    fi

    return 1
}

SYN_BIN="$(find_syn_bin || true)"
if [[ -z "$SYN_BIN" ]]; then
    echo "Could not find built syn binary. Run: riot build syn"
    exit 1
fi

PASSED=0
FAILED=0
SKIPPED=0

cd "$SCRIPT_DIR"

for ml_file in tests/fixtures/*.ml; do
    expected_file="${ml_file}.expected"
    
    # Skip if not a .ml file
    [[ ! -f "$ml_file" ]] && continue
    
    # Parse the file
    result=$("$SYN_BIN" parse "$ml_file" --json 2>&1 | tail -1)
    
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
