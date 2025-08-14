#!/usr/bin/env bash
set -euo pipefail

OCAML_VERSION="5.3.0"

# Determine host triplet
OS=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)
case "${OS}" in
  darwin) HOST_OS="apple-darwin" ;;
  linux) HOST_OS="unknown-linux-gnu" ;;
  *) HOST_OS="${OS}" ;;
esac
case "${ARCH}" in
  arm64|aarch64) HOST_ARCH="aarch64" ;;
  x86_64) HOST_ARCH="x86_64" ;;
  *) HOST_ARCH="${ARCH}" ;;
esac
HOST_TRIPLET="${HOST_ARCH}-${HOST_OS}"

TARBALL="ocaml-platform-${OCAML_VERSION}-${HOST_TRIPLET}.tar.gz"
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

# Install base dependencies
log_info "Installing base dependencies..."
opam install --switch=${OCAML_VERSION} -y dune ocamlfind

# Build ocamlformat
log_info "Building ocamlformat..."
pushd ocamlformat
  opam install --switch=${OCAML_VERSION} -y ./ocamlformat.opam
  opam exec --switch=${OCAML_VERSION} -- dune build --release @install
  cp _build/install/default/bin/ocamlformat ${BIN_DIR}
  cp _build/install/default/bin/ocamlformat-rpc ${BIN_DIR}
popd

# Build odoc  
log_info "Building odoc..."
pushd odoc
  opam install --switch=${OCAML_VERSION} -y ./odoc-md.opam ./sherlodoc.opam ./odoc-parser.opam ./odoc.opam ./odoc-driver.opam
  opam exec --switch=${OCAML_VERSION} -- dune build --release @install
  cp _build/install/default/bin/odoc ${BIN_DIR}
  cp _build/install/default/bin/odoc_driver ${BIN_DIR}
  cp _build/install/default/bin/odoc-md ${BIN_DIR}
  cp _build/install/default/bin/sherlodoc ${BIN_DIR}
popd

# Build ocaml-lsp-server
log_info "Building ocaml-lsp-server..."
pushd ocaml-lsp-server
  # Apply tusk patches
  git apply ../patches/ocaml-lsp-server-tusk.patch 2>/dev/null || true
  git apply ../patches/ocaml-lsp-server-ocamlformat-rpc.patch 2>/dev/null || true
  opam install --switch=${OCAML_VERSION} -y ./ocaml-lsp-server.opam
  opam exec --switch=${OCAML_VERSION} -- dune build --release @install
  cp _build/install/default/bin/ocamllsp ${BIN_DIR}
popd

# Create tarball
log_info "Creating tarball..."
tar czf ${TARBALL} -C ${DIST_DIR} .

log_info "Build complete! Tarball created: ${TARBALL}"
