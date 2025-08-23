#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OCAML_SRC_DIR="$SCRIPT_DIR/../compiler"

echo "🧹 Cleaning OCaml source directory..."
cd "$OCAML_SRC_DIR"

# Clean all build artifacts
make distclean || true
rm -f .depend.* || true
rm -f */depend || true
rm -f */.depend || true
rm -f Makefile.config || true
rm -f config.log config.status || true

# Remove generated files
find . -name "*.cmi" -delete || true
find . -name "*.cmo" -delete || true
find . -name "*.cmx" -delete || true
find . -name "*.cma" -delete || true
find . -name "*.cmxa" -delete || true
find . -name "*.a" -delete || true
find . -name "*.o" -delete || true

echo "✅ OCaml source cleaned"