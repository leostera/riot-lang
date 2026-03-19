# riot-ffi AGENTS

`riot-ffi` is the ergonomic Rust-facing facade over the low-level core.

## Rules

1. Keep the prelude and conversion APIs ergonomic but predictable.
2. Re-export only the pieces needed by library authors.
3. If ergonomics require changing low-level representation, update `riot-core` guidance and callers together.

## Validate

`cargo check -p riot-ffi`
