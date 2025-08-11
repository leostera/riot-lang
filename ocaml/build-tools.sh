#!/usr/bin/env bash
set -euo pipefail

# Build OCaml development tools
# This script builds ocamlformat, odoc, and ocaml-lsp-server from source

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Configuration
OCAML_VERSION="${OCAML_VERSION:-5.3.0}"
BUILD_DIR="${BUILD_DIR:-$SCRIPT_DIR/_build}"
PREFIX="${PREFIX:-$SCRIPT_DIR/_install}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if opam is available
check_opam() {
    if ! command -v opam &> /dev/null; then
        log_error "opam is not installed. Please install opam first."
        exit 1
    fi
}

# Check if required OCaml version is available
check_ocaml() {
    if ! opam switch list 2>/dev/null | grep -q "$OCAML_VERSION"; then
        log_warn "OCaml $OCAML_VERSION switch not found. Creating it..."
        opam switch create "$OCAML_VERSION" "ocaml-base-compiler.$OCAML_VERSION" -y || {
            log_info "Switch $OCAML_VERSION already exists or creation failed, continuing..."
        }
    else
        log_info "Using existing OCaml $OCAML_VERSION switch"
    fi
}

# Initialize submodules if needed
init_submodules() {
    log_info "Checking submodules..."
    
    if [ ! -d "ocaml-lsp-server/.git" ] || [ ! -d "odoc/.git" ] || [ ! -d "ocamlformat/.git" ]; then
        log_info "Initializing submodules..."
        (cd .. && git submodule update --init --recursive ocaml/)
    else
        log_info "Submodules already initialized"
    fi
}

# Install build dependencies
install_deps() {
    log_info "Installing build dependencies..."
    
    # Install dependencies for each tool by parsing their opam files
    log_info "Installing ocamlformat dependencies..."
    if [ -f "$SCRIPT_DIR/ocamlformat/ocamlformat.opam" ]; then
        (cd "$SCRIPT_DIR/ocamlformat" && opam install . --deps-only -y) || true
    fi
    
    log_info "Installing odoc dependencies..."
    if [ -f "$SCRIPT_DIR/odoc/odoc.opam" ]; then
        (cd "$SCRIPT_DIR/odoc" && opam install . --deps-only -y) || true
    fi
    
    log_info "Installing ocaml-lsp-server dependencies..."
    if [ -f "$SCRIPT_DIR/ocaml-lsp-server/ocaml-lsp-server.opam" ]; then
        (cd "$SCRIPT_DIR/ocaml-lsp-server" && opam install . --deps-only -y) || true
    fi
}

# Build a tool
build_tool() {
    local tool_name=$1
    local tool_dir=$2
    local install_target=${3:-@install}
    
    log_info "Building $tool_name..."
    
    local full_tool_dir="$SCRIPT_DIR/$tool_dir"
    
    if [ ! -d "$full_tool_dir" ]; then
        log_error "Directory $full_tool_dir not found!"
        return 1
    fi
    
    # Save current directory
    local original_dir=$(pwd)
    
    cd "$full_tool_dir"
    
    # Clean previous builds
    rm -rf _build
    
    # Build - use --root to specify this is the project root
    eval $(opam env --switch="$OCAML_VERSION")
    dune build --root . $install_target
    
    # Install to prefix
    if [ -d "_build/install/default" ]; then
        log_info "Installing $tool_name to $PREFIX..."
        mkdir -p "$PREFIX"
        cp -r _build/install/default/* "$PREFIX/" || true
    fi
    
    # Return to original directory
    cd "$original_dir"
    
    log_info "$tool_name built successfully"
}

# Build all tools
build_all() {
    # Create build and install directories
    mkdir -p "$BUILD_DIR" "$PREFIX"
    
    # Build each tool
    build_tool "ocamlformat" "ocamlformat"
    build_tool "odoc" "odoc" 
    build_tool "ocaml-lsp-server" "ocaml-lsp-server"
}

# Create distribution tarball
create_dist() {
    local os_name=$(uname -s | tr '[:upper:]' '[:lower:]')
    local arch=$(uname -m)
    local dist_name="ocaml-tools-${OCAML_VERSION}-${os_name}-${arch}"
    local dist_file="${BUILD_DIR}/${dist_name}.tar.gz"
    
    log_info "Creating distribution package: $dist_file"
    
    if [ ! -d "$PREFIX" ]; then
        log_error "Install directory $PREFIX does not exist!"
        return 1
    fi
    
    # Save current directory
    local original_dir=$(pwd)
    
    cd "$PREFIX"
    tar czf "$dist_file" .
    cd "$original_dir"
    
    log_info "Distribution package created: $dist_file"
    echo "$dist_file"
}

# Main execution
main() {
    log_info "Starting OCaml tools build..."
    log_info "OCaml version: $OCAML_VERSION"
    log_info "Build directory: $BUILD_DIR"
    log_info "Install prefix: $PREFIX"
    
    check_opam
    check_ocaml
    init_submodules
    
    # Switch to the correct OCaml version
    eval $(opam env --switch="$OCAML_VERSION")
    
    install_deps
    build_all
    
    # List what was built
    log_info "Build complete! Installed binaries:"
    if [ -d "$PREFIX/bin" ]; then
        ls -la "$PREFIX/bin/" | grep -E "ocamlformat|odoc|ocamllsp" || true
    fi
    
    # Create distribution if requested
    if [ "${CREATE_DIST:-0}" = "1" ]; then
        create_dist
    fi
}

# Allow sourcing for testing
if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
    main "$@"
fi