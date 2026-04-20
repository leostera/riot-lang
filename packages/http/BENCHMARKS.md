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
| `small request` | `7.42 us` | `6.76 us` | `5.22 us` |
| `1 KiB body` | `11.51 us` | `10.31 us` | `11.12 us` |
| `100 KiB body` | `53.68 us` | `15.77 us` | `8.00 us` |
| `1 MiB body` | `187.13 us` | `10.27 us` | `8.20 us` |
| `10 MiB body` | `1.32 ms` | `10.40 us` | `8.00 us` |
| `many headers` | `204.35 us` | `186.28 us` | `176.78 us` |
| `github navigation request` | `174.53 us` | `170.85 us` | `165.33 us` |

### Reader-Fed

| Shape | Strings (`parse`) | Slices (`parse_slice`) | Borrowed (`parse_slices`) |
| --- | ---: | ---: | ---: |
| `small request` | `12.64 us` | `13.58 us` | `10.23 us` |
| `1 KiB body` | `38.33 us` | `20.07 us` | `15.15 us` |
| `100 KiB body` | `159.02 us` | `182.90 us` | `173.85 us` |
| `1 MiB body` | `843.93 us` | `540.13 us` | `697.27 us` |
| `10 MiB body` | `5.67 ms` | `3.90 ms` | `3.15 ms` |
| `many headers` | `203.50 us` | `209.30 us` | `188.01 us` |
| `github navigation request` | `176.86 us` | `182.47 us` | `169.39 us` |

- The direct slice path is now essentially flat in parser-only body-heavy cases because the request
  parser no longer forces the body into a heap string. At `10 MiB`, `parse_slice` is `10.40 us`
  and the borrowed path is `8.00 us`.
- The public string path still scales with body size because it must first copy the full request
  string into an `IoSlice`. The lazy `Http.Body.t` change removed the second large copy, which is
  why `parse` dropped from `2.86 ms` to `1.32 ms` at `10 MiB`.
- Reader-fed numbers are now mostly ingestion plus head parsing. On large bodies, the owned slice
  path is the pragmatic default for real server traffic: `3.90 ms` at `10 MiB` versus `5.67 ms`
  for the public string entry point, while keeping an owned public `Request.t`.

## Request Shapes

- `small request`: `GET /health` with `Host` and `Accept` headers, no body
- `1 KiB body`: `POST /v1/data` with `Content-Length: 1024`
- `100 KiB body`: `PUT /bulk` with `Content-Length: 100000`
- `1 MiB body`: `PATCH /archive` with `Content-Length: 1000000`
- `10 MiB body`: `PATCH /archive` with `Content-Length: 10000000`
- `many headers`: `GET /headers` with `Host` plus `80` synthetic headers
- `github navigation request`: sanitized GitHub-like `GET` with a long query string, many browser
  headers, and a large synthetic `Cookie` header

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

These are the current means for `Http1.Request.parse`, which still materializes an `IoSlice`
internally before parsing.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory: small request` | `7.42 us` |
| `http1 parser in-memory: 1 KiB body` | `11.51 us` |
| `http1 parser in-memory: 100 KiB body` | `53.68 us` |
| `http1 parser in-memory: 1 MiB body` | `187.13 us` |
| `http1 parser in-memory: 10 MiB body` | `1.32 ms` |
| `http1 parser in-memory: many headers` | `204.35 us` |
| `http1 parser in-memory: github navigation request` | `174.53 us` |

### Reader-Fed

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed: small request` | `12.64 us` |
| `http1 parser reader-fed: 1 KiB body` | `38.33 us` |
| `http1 parser reader-fed: 100 KiB body` | `159.02 us` |
| `http1 parser reader-fed: 1 MiB body` | `843.93 us` |
| `http1 parser reader-fed: 10 MiB body` | `5.67 ms` |
| `http1 parser reader-fed: many headers` | `203.50 us` |
| `http1 parser reader-fed: github navigation request` | `176.86 us` |

## Current: Direct Slice Entry Point

These are the current means for `Http1.Request.parse_slice`. This path now materializes the request
head into an owned `Request.t` while keeping the body lazy on `Std.Net.Http.Body.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory slice: small request` | `6.76 us` |
| `http1 parser in-memory slice: 1 KiB body` | `10.31 us` |
| `http1 parser in-memory slice: 100 KiB body` | `15.77 us` |
| `http1 parser in-memory slice: 1 MiB body` | `10.27 us` |
| `http1 parser in-memory slice: 10 MiB body` | `10.40 us` |
| `http1 parser in-memory slice: many headers` | `186.28 us` |
| `http1 parser in-memory slice: github navigation request` | `170.85 us` |

### Reader-Fed

The direct reader-fed slice path currently reads into `Std.IO.IoBuffer` with vectored reads and
then parses `Std.IO.IoBuffer.readable`.

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed slice: small request` | `13.58 us` |
| `http1 parser reader-fed slice: 1 KiB body` | `20.07 us` |
| `http1 parser reader-fed slice: 100 KiB body` | `182.90 us` |
| `http1 parser reader-fed slice: 1 MiB body` | `540.13 us` |
| `http1 parser reader-fed slice: 10 MiB body` | `3.90 ms` |
| `http1 parser reader-fed slice: many headers` | `209.30 us` |
| `http1 parser reader-fed slice: github navigation request` | `182.47 us` |

## Current: Borrowed Slice Entry Point

These are the current means for `Http1.Request.parse_slices`, which keeps method, path, version,
headers, and body as borrowed `IoSlice` values instead of materializing a `Request.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory borrowed slice: small request` | `5.22 us` |
| `http1 parser in-memory borrowed slice: 1 KiB body` | `11.12 us` |
| `http1 parser in-memory borrowed slice: 100 KiB body` | `8.00 us` |
| `http1 parser in-memory borrowed slice: 1 MiB body` | `8.20 us` |
| `http1 parser in-memory borrowed slice: 10 MiB body` | `8.00 us` |
| `http1 parser in-memory borrowed slice: many headers` | `176.78 us` |
| `http1 parser in-memory borrowed slice: github navigation request` | `165.33 us` |

### Reader-Fed

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed borrowed slice: small request` | `10.23 us` |
| `http1 parser reader-fed borrowed slice: 1 KiB body` | `15.15 us` |
| `http1 parser reader-fed borrowed slice: 100 KiB body` | `173.85 us` |
| `http1 parser reader-fed borrowed slice: 1 MiB body` | `697.27 us` |
| `http1 parser reader-fed borrowed slice: 10 MiB body` | `3.15 ms` |
| `http1 parser reader-fed borrowed slice: many headers` | `188.01 us` |
| `http1 parser reader-fed borrowed slice: github navigation request` | `169.39 us` |

## Current Read

- The `Std.String.to_reader` optimization removed the earlier catastrophic reader-fed regression,
  and the lazy `Http.Body.t` change removed the other major cost center. The public string parse
  at `1 MiB` is now `843.93 us`, down from the old `103.78 ms` baseline and from the earlier
  eager-body `1.10 ms`.
- The additive borrowed parser path, `parse_slices`, still makes the underlying request-head parse
  cost visible. Parser-only, it stays in the low-microsecond range even on large bodies: `8.00 us`
  at `100 KiB`, `8.20 us` at `1 MiB`, and `8.00 us` at `10 MiB`.
- The direct slice path, `parse_slice`, is now close to that borrowed ceiling for large bodies
  while still returning an owned public `Request.t`: `15.77 us` at `100 KiB`, `10.27 us` at
  `1 MiB`, and `10.40 us` at `10 MiB`.
- That result makes the remaining costs clear. The slice substrate and delimiter scans are no
  longer the bottleneck. What remains is:
  - the full-input `string -> IoSlice` copy in `parse`
  - owned higher-level value construction like `Uri.t` and `Request.t`
- Small and many-header cases remain in the same general range across entry points. The sanitized
  GitHub-like request behaves more like the many-header shape than the body-heavy shapes: it
  stresses long request lines and large header values rather than body materialization.
