#!/bin/bash
set -e

# Simple OCaml cross-compilation using existing compiler and our sysroot
# Usage: ./cross_compile_ocaml.sh <triplet> <source.ml> <output>

if [ $# -ne 3 ]; then
    echo "Usage: $0 <triplet> <source.ml> <output>"
    echo "Example: $0 x86_64-linux-gnu test.ml test_linux"
    exit 1
fi

TRIPLET="$1"
SOURCE="$2"
OUTPUT="$3"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SYSROOT_DIR="$SCRIPT_DIR/$TRIPLET/sysroot"
OCAMLOPT="$HOME/.tusk/toolchains/5.3.0/bin/ocamlopt"

if [ ! -f "$OCAMLOPT" ]; then
    echo "❌ Error: OCaml compiler not found at $OCAMLOPT"
    exit 1
fi

if [ ! -d "$SYSROOT_DIR" ]; then
    echo "❌ Error: Sysroot not found at $SYSROOT_DIR"
    echo "   Run: ./$TRIPLET/make_sysroot.sh first"
    exit 1
fi

echo "🔨 Cross-compiling OCaml program..."
echo "   Source: $SOURCE"
echo "   Target: $TRIPLET"
echo "   Output: $OUTPUT"

# Set up cross-compilation environment
export CC="clang --target=$TRIPLET -fuse-ld=lld --sysroot=$SYSROOT_DIR"
export AS="$CC -c"

# Compile OCaml to native code
echo "   → Compiling with ocamlopt..."
$OCAMLOPT -verbose \
    -cc "$CC" \
    -ccopt "-fuse-ld=lld" \
    -o "$OUTPUT" \
    "$SOURCE"

echo "✅ Cross-compilation complete!"
echo "📁 Output: $OUTPUT"
file "$OUTPUT"