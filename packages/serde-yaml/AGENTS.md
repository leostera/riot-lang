# serde-yaml AGENTS

`serde-yaml` owns Riot's schema-driven YAML codec on top of `serde`.

## Rules

1. Keep the public API aligned with the other serde format packages: `to_string`, `to_writer`, `from_string`, and `from_reader`.
2. YAML can carry any top-level value. Do not force a table-shaped document like TOML.
3. Keep v1 focused on the subset the package emits and decodes reliably: scalars, mappings, sequences, `null`, and tagged variants. Do not claim anchor, alias, or multi-document support.
4. Render strings and field keys quoted so roundtrips stay unambiguous.
5. Prefer stable block-style output for mappings and sequences instead of trying to mirror every YAML surface form.

## Validate

`timeout 30 riot build serde-yaml`
`timeout 30 riot test -p serde-yaml`
`timeout 60 riot bench -p serde-yaml`
