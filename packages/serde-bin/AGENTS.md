# serde-bin AGENTS

`serde-bin` owns Riot's compact schema-driven binary encoding on top of `serde`.

## Rules

1. Keep the wire format positional and schema-driven, with structure coming from the schema.
2. Treat record field order and variant order as compatibility-sensitive encoding contracts.
3. Keep `from_string`/`from_reader` strict about trailing bytes. Prefix-decoding behavior belongs in `decode_prefix`.
4. Model unknown-field skipping through explicit schema support elsewhere.
5. Benchmark binary changes against `Stdlib.Marshal` when performance-sensitive behavior changes.
