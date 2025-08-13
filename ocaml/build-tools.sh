#!/bin/bash
# Build OCaml platform tools with patches for tusk integration

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PATCHES_DIR="$SCRIPT_DIR/patches"

echo "=== Building OCaml Platform Tools with Tusk Patches ==="
echo ""

# Function to apply patches
apply_patches() {
    local submodule=$1
    local patch_pattern=$2
    
    echo "Checking for patches in $PATCHES_DIR matching $patch_pattern..."
    
    for patch in $PATCHES_DIR/$patch_pattern*.patch; do
        if [ -f "$patch" ]; then
            echo "Applying patch: $(basename $patch)"
            cd "$SCRIPT_DIR/$submodule"
            
            # Check if patch is already applied
            if git apply --check "$patch" 2>/dev/null; then
                git apply "$patch"
                echo "  ✓ Patch applied successfully"
            else
                echo "  ⚠ Patch already applied or doesn't apply cleanly"
            fi
            
            cd "$SCRIPT_DIR"
        fi
    done
}

# Apply patches to ocaml-lsp-server
echo "1. Applying patches to ocaml-lsp-server..."
apply_patches "ocaml-lsp-server" "ocaml-lsp-server"
echo ""

# Setup PATH to include tusk toolchain
TOOLCHAIN_DIR="$HOME/.tusk/toolchains/5.3.0"
if [ -d "$TOOLCHAIN_DIR/bin" ]; then
    export PATH="$TOOLCHAIN_DIR/bin:$PATH"
    echo "Using tusk toolchain at $TOOLCHAIN_DIR"
    echo ""
fi

# Build ocaml-lsp-server
echo "2. Building ocaml-lsp-server..."
cd "$SCRIPT_DIR/ocaml-lsp-server"

# Check if dune is available
if command -v dune >/dev/null 2>&1; then
    echo "  Using dune to build ocaml-lsp-server..."
    echo "  OCaml compiler: $(which ocamlc || echo 'not found')"
    
    # Check if ocamlc is available
    if ! command -v ocamlc >/dev/null 2>&1; then
        echo "  ⚠ ocamlc not found. Please ensure OCaml is installed"
        echo "    Install with: ./bootstrap.py (to install tusk toolchain)"
        exit 1
    fi
    
    echo "  Note: ocaml-lsp-server requires many dependencies (yojson, stdune, merlin-lib, etc.)"
    echo "        If build fails, install dependencies with: opam install ocaml-lsp-server --deps-only"
    echo "        Or use a pre-built binary from opam"
    echo ""
    
    # Try to build, but don't fail if dependencies are missing
    if dune build @install 2>/dev/null; then
        echo "  ✓ Build complete"
    else
        echo "  ⚠ Build failed - likely missing dependencies"
        echo "    The patch has been applied. To complete the build:"
        echo "    1. Install dependencies: opam install yojson stdune merlin-lib dune-rpc fiber"
        echo "    2. Re-run this script"
        echo "    Or use pre-built: opam install ocaml-lsp-server"
    fi
else
    echo "  ⚠ dune not found. Please install dune to build ocaml-lsp-server"
    echo "    You can install it with: opam install dune"
fi
echo ""

# Build ocamlformat if needed
if [ -d "$SCRIPT_DIR/ocamlformat" ]; then
    echo "3. Building ocamlformat..."
    cd "$SCRIPT_DIR/ocamlformat"
    
    # Apply any ocamlformat patches
    apply_patches "ocamlformat" "ocamlformat"
    
    if command -v dune >/dev/null 2>&1; then
        echo "  Using dune to build ocamlformat..."
        dune build @install
        echo "  ✓ Build complete"
    else
        echo "  ⚠ Skipping ocamlformat (dune not found)"
    fi
    echo ""
fi

# Build odoc if needed
if [ -d "$SCRIPT_DIR/odoc" ]; then
    echo "4. Building odoc..."
    cd "$SCRIPT_DIR/odoc"
    
    # Apply any odoc patches
    apply_patches "odoc" "odoc"
    
    if command -v dune >/dev/null 2>&1; then
        echo "  Using dune to build odoc..."
        dune build @install
        echo "  ✓ Build complete"
    else
        echo "  ⚠ Skipping odoc (dune not found)"
    fi
    echo ""
fi

cd "$SCRIPT_DIR"
echo "=== Build Complete ==="
echo ""
echo "Built tools are in their respective _build directories."
echo "Run ./local-install.sh to install them to ~/.tusk/toolchains/"
echo ""