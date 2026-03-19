# riot-core AGENTS

`riot-core` owns the shared value model and low-level types used by the binding layer.

## Rules

1. Treat representation changes as ABI-sensitive.
2. Keep the core crate small and dependency-light.
3. Prefer explicit conversions and typed value wrappers over magical coercions.

## Validate

`cargo check -p riot-core`
