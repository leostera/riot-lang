# OCaml Development Tools for Tusk

This directory contains git submodules for essential OCaml development tools that tusk needs but doesn't want to rewrite from scratch.

## Structure

- `ocaml-lsp-server/` - OCaml Language Server Protocol implementation
- `odoc/` - OCaml documentation generator  
- `ocamlformat/` - OCaml code formatter

## Build Process

These tools are built by CI (see `.github/workflows/build-toolchains.yml`) for each supported OCaml version and platform combination. The built binaries are uploaded as GitHub releases.

When a user installs a toolchain with tusk, it:
1. Builds the OCaml compiler from source (for system compatibility)
2. Downloads the pre-built dev tools for their platform from GitHub releases

## Supported Platforms

- Linux x86_64
- Linux ARM64
- macOS x86_64 (Intel)
- macOS ARM64 (Apple Silicon)

## Adding New Tools

1. Add as git submodule: `git submodule add <repo-url> <tool-name>`
2. Update the CI workflow to build the new tool
3. Update `toolchains.ml` to check for and download the new tool