#!/bin/bash
set -e

# Usage: ./make_cross_compiler.sh <triplet>
# Example: ./make_cross_compiler.sh x86_64-linux-gnu

if [ $# -ne 1 ]; then
    echo "Usage: $0 <triplet>"
    echo "Example: $0 x86_64-linux-gnu"
    exit 1
fi

TRIPLET="$1"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OCAML_SRC_DIR="$SCRIPT_DIR/../compiler"
TARGET_DIR="$SCRIPT_DIR/$TRIPLET"
SYSROOT_DIR="$TARGET_DIR/sysroot"
INSTALL_DIR="$TARGET_DIR"

echo "=== Building OCaml Cross-Compiler for $TRIPLET ==="
echo

# Validate inputs
if [ ! -d "$SYSROOT_DIR" ]; then
    echo "❌ Error: Sysroot not found at $SYSROOT_DIR"
    echo "   Run: ./$TRIPLET/make_sysroot.sh first"
    exit 1
fi

if [ ! -d "$OCAML_SRC_DIR" ]; then
    echo "❌ Error: OCaml source not found at $OCAML_SRC_DIR"
    echo "   Expected: ocaml/compiler directory with OCaml source"
    exit 1
fi

echo "📂 Directories:"
echo "   OCaml source: $OCAML_SRC_DIR"
echo "   Target sysroot: $SYSROOT_DIR"
echo "   Install prefix: $INSTALL_DIR"
echo

# Setup cross-compilation environment
export CC="clang --target=$TRIPLET -fuse-ld=lld --sysroot=$SYSROOT_DIR"
export AS="clang --target=$TRIPLET -fuse-ld=lld --sysroot=$SYSROOT_DIR -c"
export AR="llvm-ar"
export RANLIB="llvm-ranlib"
export STRIP="llvm-strip"

echo "🔧 Cross-compilation environment:"
echo "   CC=$CC"
echo "   Target: $TRIPLET"
echo "   Sysroot: $SYSROOT_DIR"
echo

# Create build directory
BUILD_DIR="$TARGET_DIR/build"
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

echo "📋 Step 1: Cleaning and copying OCaml source to build directory..."
# Clean the source first
"$SCRIPT_DIR/clean_ocaml_source.sh"

cp -r "$OCAML_SRC_DIR"/* "$BUILD_DIR/"
cd "$BUILD_DIR"

echo "🔧 Step 2: Building host compiler first (for bootstrapping)..."
# OCaml cross-compilation needs a host compiler to bootstrap
HOST_BUILD_DIR="$TARGET_DIR/host-build"
rm -rf "$HOST_BUILD_DIR"
mkdir -p "$HOST_BUILD_DIR"
cp -r "$OCAML_SRC_DIR"/* "$HOST_BUILD_DIR/"

cd "$HOST_BUILD_DIR"
echo "   → Configuring host compiler..."
./configure --prefix="$HOST_BUILD_DIR/install" --disable-ocamldoc --disable-debugger 2>&1 | tee host-configure.log

echo "   → Building host compiler (this takes a few minutes)..."
# Build just what we need for bootstrapping
make world 2>&1 | tee host-build.log
make install 2>&1 | tee host-install.log

# Back to cross-compiler build directory
cd "$BUILD_DIR"

echo "🔧 Step 3: Configuring OCaml cross-compiler..."
./configure \
    --host="$TRIPLET" \
    --target="$TRIPLET" \
    --prefix="$INSTALL_DIR" \
    --with-target-bindir="$INSTALL_DIR/bin" \
    --disable-ocamldoc \
    --disable-debugger \
    --disable-instrumented-runtime \
    --enable-imprecise-c99-float-ops \
    CAMLRUN="$HOST_BUILD_DIR/install/bin/ocamlrun" \
    OCAMLC="$HOST_BUILD_DIR/install/bin/ocamlc" \
    OCAMLOPT="$HOST_BUILD_DIR/install/bin/ocamlopt" \
    OCAMLDEP="$HOST_BUILD_DIR/install/bin/ocamldep" \
    OCAMLLEX="$HOST_BUILD_DIR/install/bin/ocamllex" \
    OCAMLYACC="$HOST_BUILD_DIR/install/bin/ocamlyacc" \
    2>&1 | tee configure.log

echo "📋 Configuration summary:"
grep -E "(C compiler|target|prefix)" configure.log || true
echo

echo "🔨 Step 4: Building OCaml cross-compiler..."
echo "   This will take several minutes..."

# First, build dependencies and bootstrap
echo "   → Building dependencies..."
make depend 2>&1 | tee depend.log || true

echo "   → Building world..."
# Create empty .depend.menhir to satisfy the Makefile
touch .depend.menhir

# Build the core components
make core 2>&1 | tee build.log

echo "📦 Step 5: Installing OCaml cross-compiler..."
make install 2>&1 | tee install.log

echo "✅ Step 6: Verifying installation..."
if [ -f "$INSTALL_DIR/bin/ocamlopt" ]; then
    echo "   ✅ ocamlopt: $INSTALL_DIR/bin/ocamlopt"
    file "$INSTALL_DIR/bin/ocamlopt"
else
    echo "   ❌ ocamlopt not found"
fi

if [ -f "$INSTALL_DIR/bin/ocamlc" ]; then
    echo "   ✅ ocamlc: $INSTALL_DIR/bin/ocamlc"
    file "$INSTALL_DIR/bin/ocamlc"
else
    echo "   ❌ ocamlc not found"
fi

echo
echo "🎉 OCaml cross-compiler build complete!"
echo "📁 Installed in: $INSTALL_DIR"
echo "🚀 Test with: $INSTALL_DIR/bin/ocamlopt -version"
echo
echo "📋 Directory structure:"
find "$INSTALL_DIR" -maxdepth 2 -type d | head -10