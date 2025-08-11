#!/usr/bin/env bash
set -euo pipefail

OCAML_VERSION="5.3.0"
TARBALL="ocaml-platform-${OCAML_VERSION}.tar.gz"
DIST_DIR="dist"
BIN_DIR="../${DIST_DIR}/bin"

log_info() {
    echo "[INFO] $1"
}

# Clean and create dist directory
log_info "Cleaning and creating dist directory..."
rm -rf dist
mkdir -p dist/bin

# Setup OCaml 0 switch
log_info "Setting up OCaml ${OCAML_VERSION} switch..."
opam switch ${OCAML_VERSION} 2>/dev/null \
  || opam switch create ${OCAML_VERSION} ocaml-base-compiler.${OCAML_VERSION}
eval $(opam env --switch=${OCAML_VERSION})

# Install base dependencies
log_info "Installing base dependencies..."
opam install -y dune ocamlfind

# Build ocamlformat
log_info "Building ocamlformat..."
pushd ocamlformat
  opam install -y ./ocamlformat.opam
  dune build --release @install
  cp _build/install/default/bin/ocamlformat ${BIN_DIR}
popd

# Build odoc  
log_info "Building odoc..."
pushd odoc
  opam install -y ./odoc-md.opam ./sherlodoc.opam ./odoc-parser.opam ./odoc.opam ./odoc-driver.opam
  dune build --release @install
  cp _build/install/default/bin/odoc ${BIN_DIR}
popd

# Build ocaml-lsp-server
log_info "Building ocaml-lsp-server..."
pushd ocaml-lsp-server
  opam install -y ./ocaml-lsp-server.opam
  dune build --release @install
  cp _build/install/default/bin/ocamllsp ${BIN_DIR}
popd

# Create tarball
log_info "Creating tarball..."
tar czf ${TARBALL} -C ${DIST_DIR} .

log_info "Build complete! Tarball created: ${TARBALL}"
