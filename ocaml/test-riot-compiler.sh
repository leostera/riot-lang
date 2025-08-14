#!/bin/bash
set -euo pipefail

COMPILER_VERSION="5.3.0+riot"
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
"$TOOLCHAIN_DIR/bin/ocamlc" -o test-instrumentation "$SCRIPT_DIR/test-instrumentation.ml"

# Run test program
echo "🏃 Running test program..."
echo ""
./test-instrumentation

# Clean up
rm -f test-instrumentation test-instrumentation.cmi test-instrumentation.cmo

echo ""
echo "💡 To inspect the generated code with instrumentation:"
echo "   $TOOLCHAIN_DIR/bin/ocamlc -dparsetree test-instrumentation.ml"