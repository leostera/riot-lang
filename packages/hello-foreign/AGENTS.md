# hello-foreign AGENTS

`hello-foreign` is the OCaml to Rust FFI smoke test.

## Rules

1. Keep this package simple. Its main job is verifying the native binding path still works.
2. Coordinate changes here with `native/hello-rust`.
3. Avoid turning the smoke test into a feature-rich example app.

## Validate

`timeout 30 riot build hello-foreign`
