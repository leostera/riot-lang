#!/bin/bash
file=$1
if [ -z "$file" ]; then
    echo "Usage: $0 <file.ml|.mli>"
    exit 1
fi

cd "$(dirname "$0")"
tmpdir=$(mktemp -d)
trap "rm -rf $tmpdir" EXIT

tusk syn parse "$file" --emit tokens > "$tmpdir/tokens.json" 2>&1
tusk syn parse "$file" --emit green-tree > "$tmpdir/green.json" 2>&1
tusk syn parse "$file" --emit red-tree > "$tmpdir/red.json" 2>&1

python3 tests/verify_parse.py "$file" "$tmpdir/tokens.json" "$tmpdir/green.json" "$tmpdir/red.json"
