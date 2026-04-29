# serde-toml AGENTS

`serde-toml` owns Riot's schema-driven TOML codec on top of `serde`.

## Rules

1. Keep the public API aligned with the other serde format packages: `to_string`, `to_writer`, `from_string`, and `from_reader`.
2. TOML documents are table-shaped at the top level.
3. Render scalar keys before nested tables and arrays of tables so output stays stable and idiomatic.
4. Arrays of tables should be used for non-empty sequences of records. Empty record sequences must render as inline `[]` so they can roundtrip.
5. Keep parser and renderer support focused on the subset the package actually encodes and decodes reliably.
