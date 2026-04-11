# serde-bson AGENTS

`serde-bson` owns Riot's schema-driven BSON codec on top of `serde`.

## Rules

1. Keep the public API aligned with the other serde format packages: `to_string`, `to_writer`, `from_string`, and `from_reader`.
2. BSON bytes are always document-rooted. Do not hide a synthetic wrapper for top-level scalar values.
3. Keep v1 focused on the BSON subset the package emits and decodes reliably: documents, arrays, strings, booleans, null, int32, int64, and doubles.
4. Arrays are represented on the wire as BSON arrays, not generic documents; validate their numeric-key layout on decode.
5. Preserve field order on encode and accept both `Int32` and `Int64` when decoding OCaml `int` values within range.

## Validate

`timeout 30 riot build serde-bson`
`timeout 60 riot test -p serde-bson`
`timeout 60 riot bench -p serde-bson`
