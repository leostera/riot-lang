# serde-urlencoded AGENTS

`serde-urlencoded` owns Riot's flat `application/x-www-form-urlencoded` encoding on top of `serde`.

## Rules

1. Keep the format flat.
2. Repeated keys represent sequences. Omitted fields represent absent optional values.
3. Use the shared `Net.Uri` form-encoding helpers for percent-encoding logic.
4. Support flat records first; expand the package contract explicitly before adding nested records or payload-carrying variants.
