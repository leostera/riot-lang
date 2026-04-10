# Native Crates

The native layer kept in this repo is the Rust binding stack for Riot.

## Kept crates

### `riot-core`

Low-level value representation and core runtime-facing types for Rust bindings.

### `riot-derive`

Proc-macro support for deriving conversions to and from `riot-core::Value`.

### `riot-ffi`

Ergonomic Rust-facing facade that re-exports `riot-core` plus derive support.

### `riot-bindgen`

Tooling for generating OCaml bindings from Rust crates.

### `hello-rust`

A minimal foreign-dependency smoke test used by `packages/hello-foreign`.

### `serde-json-bench`

A tiny standalone benchmark binary for comparing Rust `serde_json` read and
write throughput against Riot's `serde-json` benchmarks using the same fixture
files.

## Current dependency shape

```text
hello-rust
  └── riot-ffi
        ├── riot-core
        └── riot-derive

riot-bindgen
  ├── riot-core
  └── riot-ffi
```

## What was removed

These legacy experiments are no longer part of the active native workspace:

- `example-lib`
- the old bytecode runtime experiments
- the old native runner experiments
