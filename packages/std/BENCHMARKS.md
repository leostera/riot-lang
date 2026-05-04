# Std Benchmarks

This file tracks additive parser experiments in `std` before they replace the
string-first defaults.

Date:
- 2026-04-20

Commands:

```sh
timeout 300 riot bench std:std_data_json_bench --json
```

Notes:
- `Json.from_string` is the current baseline parser.
- `JsonStream.from_string` exercises the new cursor-based parser after one
  explicit copy into `IoSlice`.
- `JsonStream.from_slice` removes that front-door copy and parses directly from
  `IoSlice`.

## JSON Parsing

| Shape | `Json.from_string` | `JsonStream.from_string` | `JsonStream.from_slice` |
| --- | ---: | ---: | ---: |
| `small object` | `1.73 us` | `2.70 us` | `2.72 us` |
| `1 KiB numeric array` | `89.30 us` | `147.48 us` | `137.36 us` |
| `100 KiB numeric array` | `7.74 ms` | `14.58 ms` | `14.74 ms` |
| `1 MiB numeric array` | `63.65 ms` | `105.20 ms` | `105.27 ms` |
| `1 MiB string payload` | `41.22 ms` | `53.99 ms` | `53.75 ms` |

## Current Read

- `JsonStream.from_slice` is a small improvement over `from_string` on the small
  object and `1 KiB` array shapes, which means the front-door copy is no longer
  the main problem there.
- On larger inputs, `JsonStream.from_slice` and `JsonStream.from_string` are
  effectively tied. The parser cost dominates, not the initial slice
  materialization.
- `Json.from_string` remains the fastest path on every current JSON benchmark.
- The experiment is still useful as a substrate check, but it does not justify
  replacing `Std.Data.Json` yet.
