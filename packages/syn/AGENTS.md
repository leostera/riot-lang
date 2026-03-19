# syn AGENTS

`syn` is the OCaml lexer, parser, CST, and diagnostics layer.

## Rules

1. Preserve lossless parsing. Token and trivia retention matter.
2. Parser recovery changes are user-facing because tooling builds on diagnostics.
3. Keep syntax tree changes coordinated with any tooling that consumes `syn`, especially `tusk-fix` and `tusk-eval`.
4. Prefer explicit syntax kinds and spans over inferred structure.

## Validate

`timeout 30 tusk build syn`
