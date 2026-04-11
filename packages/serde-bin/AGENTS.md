# serde-bin AGENTS

`serde-bin` owns Riot's compact schema-driven binary encoding on top of `serde`.

## Rules

1. Keep the wire format positional and schema-driven. Do not add field names or self-describing metadata to the encoded bytes.
2. Treat record field order and variant order as compatibility-sensitive encoding contracts.
3. Keep `of_string`/`of_reader` strict about trailing bytes. Prefix-decoding behavior belongs in `decode_prefix`.
4. Do not pretend generic `skip_any` is available for this format. Unknown-field skipping needs explicit schema support elsewhere.
5. Benchmark binary changes against `Stdlib.Marshal` when performance-sensitive behavior changes.

## Validate

`timeout 30 riot build serde-bin`
