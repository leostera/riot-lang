# serde-urlencoded AGENTS

`serde-urlencoded` owns Riot's flat `application/x-www-form-urlencoded` encoding on top of `serde`.

## Rules

1. Keep the format flat. Do not invent nested field-name conventions in this package.
2. Repeated keys represent sequences. Omitted fields represent absent optional values.
3. Use the shared `Net.Uri` form-encoding helpers instead of duplicating percent-encoding logic.
4. Nested records and payload-carrying variants are intentionally unsupported unless the package contract is expanded explicitly.

## Validate

`timeout 30 riot build serde-urlencoded`
`timeout 30 riot test -p serde-urlencoded`
