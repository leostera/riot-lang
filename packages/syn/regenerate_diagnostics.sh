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

UPDATED=0

cd "$SCRIPT_DIR"

for ml_file in tests/diagnostics/*.ml; do
    [[ ! -f "$ml_file" ]] && continue
    
    diag_file="${ml_file}.diagnostic"
    [[ ! -f "$diag_file" ]] && continue
    
    # Parse and extract diagnostics
    actual=$("$SYN_BIN" parse "$ml_file" --json 2>&1 | tail -1 | jq '.diagnostics')
    
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
