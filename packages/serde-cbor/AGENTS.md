# serde-cbor AGENTS

`serde-cbor` owns Riot's schema-driven CBOR codec on top of `serde`.

## Rules

1. Keep the public API aligned with the other serde format packages: `to_string`, `to_writer`, `from_string`, and `from_reader`.
2. CBOR can carry any top-level value. Do not force a document root like BSON or TOML.
3. Keep v1 focused on the subset the package emits and decodes reliably: integers, floats, text, arrays, string-keyed maps, booleans, and null.
4. Emit definite-length items only. Decode tags liberally by unwrapping the tagged payload, but do not claim full tag semantics.
5. Preserve map field order on encode and require text keys when decoding record-shaped values.

## Validate

`timeout 30 riot build serde-cbor`
`timeout 60 riot test -p serde-cbor`
`timeout 60 riot bench -p serde-cbor`
