#!/bin/bash
# Install locally built OCaml platform tools to tusk toolchain

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOOLCHAIN_VERSION="5.3.0+riot"
TOOLCHAIN_DIR="$HOME/.tusk/toolchains/$TOOLCHAIN_VERSION"
BIN_DIR="$TOOLCHAIN_DIR/bin"

echo "=== Installing OCaml Platform Tools to Tusk Toolchain ==="
echo ""
echo "Toolchain: $TOOLCHAIN_VERSION"
echo "Install directory: $BIN_DIR"
echo ""

# Create toolchain directory if it doesn't exist
mkdir -p "$BIN_DIR"

# Function to install binary
install_binary() {
    local name=$1
    local source_path=$2
    
    if [ -f "$source_path" ]; then
        echo "Installing $name..."
        cp "$source_path" "$BIN_DIR/$name"
        chmod +x "$BIN_DIR/$name"
        echo "  ✓ Installed $name"
    else
        echo "  ⚠ $name not found at $source_path"
        return 1
    fi
}

# Function to install from dune build
install_from_dune() {
    local project=$1
    local binary=$2
    local install_name=${3:-$binary}
    
    local build_dir="$SCRIPT_DIR/$project/_build/default"
    local source_path="$build_dir/$binary"
    
    if [ -f "$source_path" ]; then
        install_binary "$install_name" "$source_path"
    else
        # Try install directory
        source_path="$build_dir/install/default/bin/$binary"
        if [ -f "$source_path" ]; then
            install_binary "$install_name" "$source_path"
        else
            echo "  ⚠ Could not find $binary in $project build directory"
            echo "    Run ./build-tools.sh first"
            return 1
        fi
    fi
}

# Install from dist/bin if available (from build-tools.sh)
if [ -d "$SCRIPT_DIR/dist/bin" ]; then
    echo "Installing from dist/bin directory..."
    for binary in ocamllsp ocamlformat ocamlformat-rpc odoc odoc_driver odoc-md sherlodoc; do
        if [ -f "$SCRIPT_DIR/dist/bin/$binary" ]; then
            echo "  Installing $binary..."
            rm -f "$BIN_DIR/$binary" 2>/dev/null || true
            cp "$SCRIPT_DIR/dist/bin/$binary" "$BIN_DIR/$binary"
            chmod +x "$BIN_DIR/$binary"
            echo "  ✓ Installed $binary"
        fi
    done
else
    # Fallback to installing from individual build directories
    echo "1. Installing ocaml-lsp-server..."
    install_from_dune "ocaml-lsp-server" "ocaml-lsp-server/bin/main.exe" "ocamllsp"
    echo ""

    # Install ocamlformat
    echo "2. Installing ocamlformat..."
    if [ -d "$SCRIPT_DIR/ocamlformat" ]; then
        install_from_dune "ocamlformat" "src/ocamlformat.exe" "ocamlformat"
    else
        echo "  ⚠ ocamlformat directory not found"
    fi
    echo ""

    # Install odoc
    echo "3. Installing odoc..."
    if [ -d "$SCRIPT_DIR/odoc" ]; then
        install_from_dune "odoc" "src/odoc/bin/main.exe" "odoc"
    else
        echo "  ⚠ odoc directory not found"
    fi
fi
echo ""

# Install tusk itself to the toolchain
echo "4. Installing tusk..."
if [ -f "$SCRIPT_DIR/../target/bootstrap/tusk" ]; then
    install_binary "tusk" "$SCRIPT_DIR/../target/bootstrap/tusk"
elif [ -f "$SCRIPT_DIR/../target/debug/tusk" ]; then
    install_binary "tusk" "$SCRIPT_DIR/../target/debug/tusk"
elif [ -f "$SCRIPT_DIR/../minitusk" ]; then
    # Bootstrap first if only minitusk exists
    echo "  Building tusk first..."
    cd "$SCRIPT_DIR/.."
    ./minitusk build
    if [ -f "$SCRIPT_DIR/../target/bootstrap/tusk" ]; then
        install_binary "tusk" "$SCRIPT_DIR/../target/bootstrap/tusk"
    fi
else
    echo "  ⚠ tusk not found. Run ./bootstrap.py first"
fi
echo ""

# Create symlinks in standard toolchain if needed
STANDARD_TOOLCHAIN="$HOME/.tusk/toolchains/5.3.0"
if [ -d "$STANDARD_TOOLCHAIN" ] && [ "$STANDARD_TOOLCHAIN" != "$TOOLCHAIN_DIR" ]; then
    echo "5. Creating symlinks in standard toolchain..."
    
    for binary in ocamllsp ocamlformat odoc; do
        if [ -f "$BIN_DIR/$binary" ]; then
            echo "  Linking $binary to standard toolchain..."
            ln -sf "$BIN_DIR/$binary" "$STANDARD_TOOLCHAIN/bin/$binary"
            echo "  ✓ Linked $binary"
        fi
    done
    echo ""
fi

# Verify installation
echo "=== Verifying Installation ==="
echo ""
echo "Installed tools in $BIN_DIR:"
ls -la "$BIN_DIR" | grep -E "ocamllsp|ocamlformat|odoc|tusk" || true
echo ""

# Test tusk
if [ -f "$BIN_DIR/tusk" ]; then
    echo "Testing tusk:"
    "$BIN_DIR/tusk" --version || echo "  (version not implemented yet)"
    echo ""
fi

# Add to PATH reminder
echo "=== Installation Complete ==="
echo ""
echo "To use these tools, add the toolchain to your PATH:"
echo "  export PATH=\"$BIN_DIR:\$PATH\""
echo ""
echo "Or use tusk directly:"
echo "  $BIN_DIR/tusk build"
echo ""