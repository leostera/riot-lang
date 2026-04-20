# HTTP Benchmarks

This file tracks the HTTP/1 request-parser migration from heap-string slicing to
`Std.IO.IoSlice`.

Date:
- 2026-04-20

Commands:

```sh
timeout 120 riot bench http:http1_parser_bench --json
timeout 120 riot bench http:http1_parser_transport_bench --json
```

Notes:
- `http1_parser_bench` measures parser-only cost on already-materialized request strings.
- `http1_parser_transport_bench` measures `Std.IO` reader accumulation plus parsing.
- The reader-fed benchmark currently uses `Std.String.to_reader` with fixed chunk sizes instead of a
  live socket or server, so it isolates request ingestion overhead without process-management noise.

## Current Summary

This is the short comparison between the owned-string parser, `Http1.Request.parse`, the
slice-owning parser entry point, `Http1.Request.parse_slice`, and the borrowed parser entry point,
`Http1.Request.parse_slices`.

### Parser Only

| Shape | Strings (`parse`) | Slices (`parse_slice`) | Borrowed (`parse_slices`) |
| --- | ---: | ---: | ---: |
| `small request` | `8.34 us` | `8.43 us` | `9.75 us` |
| `1 KiB body` | `10.42 us` | `11.96 us` | `7.75 us` |
| `100 KiB body` | `66.60 us` | `21.10 us` | `12.50 us` |
| `1 MiB body` | `394.67 us` | `89.27 us` | `7.60 us` |
| `10 MiB body` | `2.86 ms` | `1.87 ms` | `7.80 us` |
| `many headers` | `196.67 us` | `248.45 us` | `173.10 us` |

### Reader-Fed

| Shape | Strings (`parse`) | Slices (`parse_slice`) | Borrowed (`parse_slices`) |
| --- | ---: | ---: | ---: |
| `small request` | `12.70 us` | `12.39 us` | `8.88 us` |
| `1 KiB body` | `23.01 us` | `23.85 us` | `17.32 us` |
| `100 KiB body` | `196.83 us` | `202.03 us` | `186.23 us` |
| `1 MiB body` | `1.10 ms` | `583.13 us` | `648.53 us` |
| `10 MiB body` | `7.69 ms` | `4.96 ms` | `5.46 ms` |
| `many headers` | `208.00 us` | `223.40 us` | `209.98 us` |

- For parser-only work, the borrowed path makes the underlying parser cost visible. It stays
  effectively flat with body size because it never materializes the body.
- The public string path scales poorly with body size because it does two large boundary copies:
  `parse` first copies the full request string into an `IoSlice`, then `parse_slice` copies the
  body slice back out to a heap string.
- For reader-fed work, the large regression from `Std.String.to_reader` is gone. The remaining
  differences are now mostly ownership costs and ordinary benchmark variance, not the old read-loop
  bug.

## Request Shapes

- `small request`: `GET /health` with `Host` and `Accept` headers, no body
- `1 KiB body`: `POST /v1/data` with `Content-Length: 1024`
- `100 KiB body`: `PUT /bulk` with `Content-Length: 100000`
- `1 MiB body`: `PATCH /archive` with `Content-Length: 1000000`
- `10 MiB body`: `PATCH /archive` with `Content-Length: 10000000`
- `many headers`: `GET /headers` with `Host` plus `80` synthetic headers

## Baseline: Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory: small request` | `3.46 us` |
| `http1 parser in-memory: 1 KiB body` | `12.30 us` |
| `http1 parser in-memory: 100 KiB body` | `751.92 us` |
| `http1 parser in-memory: 1 MiB body` | `3.67 ms` |
| `http1 parser in-memory: many headers` | `157.77 us` |

## Baseline: Reader-Fed

Chunk sizes:
- small request: `32`
- `1 KiB` body: `64`
- `100 KiB` body: `256`
- `1 MiB` body: `1024`
- many headers: `64`

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed: small request` | `9.82 us` |
| `http1 parser reader-fed: 1 KiB body` | `17.42 us` |
| `http1 parser reader-fed: 100 KiB body` | `12.06 ms` |
| `http1 parser reader-fed: 1 MiB body` | `103.78 ms` |
| `http1 parser reader-fed: many headers` | `172.94 us` |

## Current: Public String Entry Point

These are the current means for `Http1.Request.parse`, which now materializes an `IoSlice`
internally before parsing.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory: small request` | `8.34 us` |
| `http1 parser in-memory: 1 KiB body` | `10.42 us` |
| `http1 parser in-memory: 100 KiB body` | `66.60 us` |
| `http1 parser in-memory: 1 MiB body` | `394.67 us` |
| `http1 parser in-memory: 10 MiB body` | `2.86 ms` |
| `http1 parser in-memory: many headers` | `196.67 us` |

### Reader-Fed

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed: small request` | `12.70 us` |
| `http1 parser reader-fed: 1 KiB body` | `23.01 us` |
| `http1 parser reader-fed: 100 KiB body` | `196.83 us` |
| `http1 parser reader-fed: 1 MiB body` | `1.10 ms` |
| `http1 parser reader-fed: 10 MiB body` | `7.69 ms` |
| `http1 parser reader-fed: many headers` | `208.00 us` |

## Current: Direct Slice Entry Point

These are the current means for `Http1.Request.parse_slice`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory slice: small request` | `8.43 us` |
| `http1 parser in-memory slice: 1 KiB body` | `11.96 us` |
| `http1 parser in-memory slice: 100 KiB body` | `21.10 us` |
| `http1 parser in-memory slice: 1 MiB body` | `89.27 us` |
| `http1 parser in-memory slice: 10 MiB body` | `1.87 ms` |
| `http1 parser in-memory slice: many headers` | `248.45 us` |

### Reader-Fed

The direct reader-fed slice path currently reads into `Std.IO.IoBuffer` with vectored reads and
then parses `Std.IO.IoBuffer.readable`.

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed slice: small request` | `12.39 us` |
| `http1 parser reader-fed slice: 1 KiB body` | `23.85 us` |
| `http1 parser reader-fed slice: 100 KiB body` | `202.03 us` |
| `http1 parser reader-fed slice: 1 MiB body` | `583.13 us` |
| `http1 parser reader-fed slice: 10 MiB body` | `4.96 ms` |
| `http1 parser reader-fed slice: many headers` | `223.40 us` |

## Current: Borrowed Slice Entry Point

These are the current means for `Http1.Request.parse_slices`, which keeps method, path, version,
headers, and body as borrowed `IoSlice` values instead of materializing a `Request.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory borrowed slice: small request` | `9.75 us` |
| `http1 parser in-memory borrowed slice: 1 KiB body` | `7.75 us` |
| `http1 parser in-memory borrowed slice: 100 KiB body` | `12.50 us` |
| `http1 parser in-memory borrowed slice: 1 MiB body` | `7.60 us` |
| `http1 parser in-memory borrowed slice: 10 MiB body` | `7.80 us` |
| `http1 parser in-memory borrowed slice: many headers` | `173.10 us` |

### Reader-Fed

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed borrowed slice: small request` | `8.88 us` |
| `http1 parser reader-fed borrowed slice: 1 KiB body` | `17.32 us` |
| `http1 parser reader-fed borrowed slice: 100 KiB body` | `186.23 us` |
| `http1 parser reader-fed borrowed slice: 1 MiB body` | `648.53 us` |
| `http1 parser reader-fed borrowed slice: 10 MiB body` | `5.46 ms` |
| `http1 parser reader-fed borrowed slice: many headers` | `209.98 us` |

## Current Read

- The `Std.String.to_reader` optimization removed the earlier catastrophic reader-fed regression.
  The `1 MiB` reader-fed public parse is now `1.10 ms`, down from the old `103.78 ms` baseline.
- The additive borrowed parser path, `parse_slices`, makes the actual request-head parsing cost
  visible. Parser-only, it stays in the low-microsecond range even on large bodies: `12.50 us` at
  `100 KiB`, `7.60 us` at `1 MiB`, and `7.80 us` at `10 MiB`.
- That result shows the current ceiling clearly: the remaining cost in `parse_slice` and `parse`
  is not delimiter scanning. It is ownership work at the boundary:
  - materializing the body string
  - constructing owned higher-level values like `Uri.t` and `Request.t`
- Small and many-header cases remain in the same general range across entry points. The slice
  substrate is no longer the bottleneck there; the remaining gains will come from reducing
  ownership conversions or by letting higher layers keep borrowed slices longer.
