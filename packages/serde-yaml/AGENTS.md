# serde-yaml AGENTS

`serde-yaml` owns Riot's schema-driven YAML codec on top of `serde`.

## Rules

1. Keep the public API aligned with the other serde format packages: `to_string`, `to_writer`, `from_string`, and `from_reader`.
2. YAML can carry any top-level value.
3. Keep v1 focused on the subset the package emits and decodes reliably: scalars, mappings, sequences, `null`, and tagged variants.
4. Render strings and field keys quoted so roundtrips stay unambiguous.
5. Prefer stable block-style output for mappings and sequences.
