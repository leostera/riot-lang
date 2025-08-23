#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SYSROOT_DIR="$SCRIPT_DIR/sysroot"

echo "Fixing absolute paths in sysroot..."

# Fix libc.so and other .so files to use relative paths
find "$SYSROOT_DIR" -name "*.so" -type f | while read -r file; do
    if [[ $(file "$file") == *"ASCII text"* ]]; then
        echo "Fixing paths in: $file"
        sed -i.bak 's|/lib/x86_64-linux-gnu/|lib/|g' "$file"
        sed -i.bak 's|/usr/lib/x86_64-linux-gnu/|lib/|g' "$file"
        sed -i.bak 's|/lib64/|lib/|g' "$file"
        rm -f "$file.bak"
    fi
done

echo "Sysroot paths fixed"