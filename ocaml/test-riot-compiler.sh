#!/bin/bash
set -euo pipefail

COMPILER_VERSION="5.3.0"
TOOLCHAIN_DIR="$HOME/.tusk/toolchains/$COMPILER_VERSION"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "🧪 Testing Riot-patched compiler..."
echo ""

# Check if compiler is installed
if [ ! -f "$TOOLCHAIN_DIR/bin/ocamlc" ]; then
    echo "❌ Riot compiler not found at $TOOLCHAIN_DIR"
    echo "   Run ./build-riot-compiler.sh first"
    exit 1
fi

# Compile test program
echo "📦 Compiling test program..."
"$TOOLCHAIN_DIR/bin/ocamlc" -o test_compiler "$SCRIPT_DIR/test_compiler.ml"

# Run test program
echo "🏃 Running test program..."
echo ""
./test_compiler

# Clean up
rm -f test_compiler test_compiler.cmi test_compiler.cmo

echo ""
echo "💡 To inspect the generated code with instrumentation:"
echo "   $TOOLCHAIN_DIR/bin/ocamlc -dparsetree test_compiler.ml"