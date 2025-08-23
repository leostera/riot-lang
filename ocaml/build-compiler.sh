#!/bin/bash
set -euo pipefail

# Configuration
COMPILER_VERSION="5.3.0"
TOOLCHAIN_DIR="$HOME/.tusk/toolchains/$COMPILER_VERSION"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$SCRIPT_DIR/compiler"

# Parse arguments
CLEAN_BUILD=false
if [ "${1:-}" = "--clean" ]; then
    CLEAN_BUILD=true
fi

echo "🔨 Building Riot-patched OCaml compiler..."
echo "   Version: $COMPILER_VERSION"
echo "   Target: $TOOLCHAIN_DIR"
echo ""

cd "$COMPILER_DIR"

# Determine build type
if [ "$CLEAN_BUILD" = true ] || [ ! -f "config.status" ]; then
    echo "📦 Performing clean build..."
    
    # Clean if requested
    if [ "$CLEAN_BUILD" = true ] && [ -f Makefile ]; then
        echo "   Cleaning previous build artifacts..."
        make clean || true
    fi
    
    # Configure
    echo "⚙️  Configuring compiler..."
    ./configure \
        --prefix="$TOOLCHAIN_DIR" \
        --disable-ocamldoc \
        --disable-debugger \
        --disable-ocamltest
    
    # Full build
    echo "🏗️  Building compiler (this may take a while)..."
    make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu) world
    
    echo "🏗️  Building native compiler..."
    make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu) opt
else
    echo "🔄 Performing incremental build..."
    echo "   (use --clean for full rebuild)"
    echo ""
    
    # Just rebuild changed files
    echo "🏗️  Rebuilding changed files..."
    make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu)
fi

# Install
echo "📥 Installing to $TOOLCHAIN_DIR..."
rm -rf "$TOOLCHAIN_DIR"
make install

# Create version markers
echo "$COMPILER_VERSION" > "$TOOLCHAIN_DIR/VERSION"
echo "Riot-patched OCaml compiler" > "$TOOLCHAIN_DIR/RIOT_PATCH"

# Test installation
echo ""
echo "✅ Testing installation..."
"$TOOLCHAIN_DIR/bin/ocamlc" -version

echo ""
echo "🎉 Riot compiler installed successfully!"
echo ""
echo "To use this compiler:"
echo "  1. Set as default: export PATH=\"$TOOLCHAIN_DIR/bin:\$PATH\""
echo "  2. Or use directly: $TOOLCHAIN_DIR/bin/ocamlc"
echo ""
echo "For tusk integration, update your workspace.toml:"
echo "  [toolchain]"
echo "  version = \"$COMPILER_VERSION\""
echo ""
echo "To test instrumentation: ./test-riot-compiler.sh"