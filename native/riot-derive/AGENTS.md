# riot-derive AGENTS

`riot-derive` owns the derive macros used by the Rust binding layer.

## Rules

1. Generated paths and crate references must stay aligned with `riot-core` and `riot-ffi`.
2. Macro output should prefer explicit, readable generated code over compact cleverness.
3. If a macro contract changes, update the smoke-test path that exercises it.

## Validate

`cargo check -p riot-derive`
