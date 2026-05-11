# human-units AGENTS

`human-units` owns the public `Human_units` module for human-readable
formatting and parsing of byte sizes and time durations.

## Rules

1. Keep byte formatting binary by default, using `KiB`, `MiB`, and related IEC suffixes.
2. Keep duration parsing compatible with common duration suffixes, including `ns`, `us`/`µs`, `ms`, `secs`, `mins`, `hrs`, `days`, `weeks`, `months`, and `years`.
3. Treat parsing failures as structured `error` values rather than exceptions.
4. Keep formatted output compact and stable; update tests when public spelling or rounding changes.
5. Prefer tiny lexers/tokenizers and internal ADTs for units before converting to concrete byte counts or durations.
