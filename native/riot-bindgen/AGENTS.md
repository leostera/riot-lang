# riot-bindgen AGENTS

`riot-bindgen` generates binding code for Rust libraries.

## Rules

1. Generated output should reflect the current `riot-core` and `riot-ffi` APIs.
2. Keep the generator deterministic so diffs stay reviewable.
3. Prefer simple generated code over compact but opaque output.

## Validate

`cargo check -p riot-bindgen`
