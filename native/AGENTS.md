# native AGENTS

The `native/` tree holds the Rust side of Riot's OCaml binding layer.

## Routing

- `native/riot-core/AGENTS.md`: shared ABI-safe value model
- `native/riot-derive/AGENTS.md`: derive macros for the binding layer
- `native/riot-ffi/AGENTS.md`: Rust-facing FFI facade and prelude
- `native/riot-bindgen/AGENTS.md`: binding generation tool
- `native/hello-rust/AGENTS.md`: example library used by `hello-foreign`

## Rules

1. Keep crate names, exported symbols, and OCaml-facing ABI changes deliberate.
2. Update both Rust and OCaml smoke tests when changing the binding surface.
3. Use `cargo check` for the Rust workspace and `riot build hello-foreign` for the end-to-end path.
