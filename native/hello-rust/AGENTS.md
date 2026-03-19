# hello-rust AGENTS

`hello-rust` is the example Rust library used by `packages/hello-foreign`.

## Rules

1. Keep it as a smoke test, not a product crate.
2. Coordinate any exported symbol or type changes with the OCaml caller.
3. Prefer tiny examples that exercise the real binding path.

## Validate

`cargo check -p hello-rust`
