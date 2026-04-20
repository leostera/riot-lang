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

This is the short comparison between the owned-string parser, `Http1.Request.parse`, and the
full-slice parser entry point, `Http1.Request.parse_slice`.

### Parser Only

| Shape | Strings (`parse`) | Slices (`parse_slice`) |
| --- | ---: | ---: |
| `small request` | `7.78 us` | `8.10 us` |
| `1 KiB body` | `11.57 us` | `12.99 us` |
| `100 KiB body` | `68.25 us` | `20.38 us` |
| `1 MiB body` | `362.13 us` | `201.67 us` |
| `many headers` | `201.88 us` | `207.68 us` |

### Reader-Fed

| Shape | Strings (`parse`) | Slices (`parse_slice`) |
| --- | ---: | ---: |
| `small request` | `12.67 us` | `13.14 us` |
| `1 KiB body` | `20.96 us` | `22.41 us` |
| `100 KiB body` | `194.22 us` | `255.90 us` |
| `1 MiB body` | `1.17 ms` | `1.68 ms` |
| `many headers` | `206.16 us` | `223.90 us` |

- For parser-only work, the slice path is materially better on large bodies because it delays
  ownership conversion.
- For reader-fed work, the public string path is now slightly better. The big reader-fed regression
  was fixed in `Std.String.to_reader`, so the remaining overhead is mostly the cost of building and
  owning the higher-level values, not the slice substrate itself.

## Request Shapes

- `small request`: `GET /health` with `Host` and `Accept` headers, no body
- `1 KiB body`: `POST /v1/data` with `Content-Length: 1024`
- `100 KiB body`: `PUT /bulk` with `Content-Length: 100000`
- `1 MiB body`: `PATCH /archive` with `Content-Length: 1000000`
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
| `http1 parser in-memory: small request` | `7.78 us` |
| `http1 parser in-memory: 1 KiB body` | `11.57 us` |
| `http1 parser in-memory: 100 KiB body` | `68.25 us` |
| `http1 parser in-memory: 1 MiB body` | `362.13 us` |
| `http1 parser in-memory: many headers` | `201.88 us` |

### Reader-Fed

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed: small request` | `12.67 us` |
| `http1 parser reader-fed: 1 KiB body` | `20.96 us` |
| `http1 parser reader-fed: 100 KiB body` | `194.22 us` |
| `http1 parser reader-fed: 1 MiB body` | `1.17 ms` |
| `http1 parser reader-fed: many headers` | `206.16 us` |

## Current: Direct Slice Entry Point

These are the current means for `Http1.Request.parse_slice`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory slice: small request` | `8.10 us` |
| `http1 parser in-memory slice: 1 KiB body` | `12.99 us` |
| `http1 parser in-memory slice: 100 KiB body` | `20.38 us` |
| `http1 parser in-memory slice: 1 MiB body` | `201.67 us` |
| `http1 parser in-memory slice: many headers` | `207.68 us` |

### Reader-Fed

The direct reader-fed slice path currently reads into `Std.IO.IoBuffer` with vectored reads and
then parses `Std.IO.IoBuffer.readable`.

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed slice: small request` | `13.14 us` |
| `http1 parser reader-fed slice: 1 KiB body` | `22.41 us` |
| `http1 parser reader-fed slice: 100 KiB body` | `255.90 us` |
| `http1 parser reader-fed slice: 1 MiB body` | `1.68 ms` |
| `http1 parser reader-fed slice: many headers` | `223.90 us` |

## Current: Borrowed Slice Entry Point

These are the current means for `Http1.Request.parse_slices`, which keeps method, path, version,
headers, and body as borrowed `IoSlice` values instead of materializing a `Request.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory borrowed slice: small request` | `6.44 us` |
| `http1 parser in-memory borrowed slice: 1 KiB body` | `8.69 us` |
| `http1 parser in-memory borrowed slice: 100 KiB body` | `14.10 us` |
| `http1 parser in-memory borrowed slice: 1 MiB body` | `10.47 us` |
| `http1 parser in-memory borrowed slice: many headers` | `177.80 us` |

### Reader-Fed

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed borrowed slice: small request` | `9.95 us` |
| `http1 parser reader-fed borrowed slice: 1 KiB body` | `17.59 us` |
| `http1 parser reader-fed borrowed slice: 100 KiB body` | `321.60 us` |
| `http1 parser reader-fed borrowed slice: 1 MiB body` | `1.23 ms` |
| `http1 parser reader-fed borrowed slice: many headers` | `198.57 us` |

## Current Read

- The `Std.String.to_reader` optimization removed the earlier catastrophic reader-fed regression.
  The `1 MiB` reader-fed public parse is now `1.27 ms`, down from the old `103.78 ms` baseline.
- The additive borrowed parser path, `parse_slices`, makes the actual request-head parsing cost
  visible. Parser-only, it stays in the low-microsecond range even on large bodies: `14.10 us` at
  `100 KiB` and `10.47 us` at `1 MiB`.
- That result shows the current ceiling clearly: the remaining cost in `parse_slice` and `parse`
  is not delimiter scanning. It is ownership work at the boundary:
  - materializing the body string
  - constructing owned higher-level values like `Uri.t` and `Request.t`
- Small and many-header cases remain in the same general range across entry points. The slice
  substrate is no longer the bottleneck there; the remaining gains will come from reducing
  ownership conversions or by letting higher layers keep borrowed slices longer.
