# HTTP Benchmarks

This file tracks the HTTP/1 request-parser migration from heap-string slicing to
`Std.IO.IoSlice`.

Date:
- 2026-04-19

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
| `http1 parser in-memory: small request` | `6.95 us` |
| `http1 parser in-memory: 1 KiB body` | `15.16 us` |
| `http1 parser in-memory: 100 KiB body` | `82.42 us` |
| `http1 parser in-memory: 1 MiB body` | `333.67 us` |
| `http1 parser in-memory: many headers` | `203.11 us` |

### Reader-Fed

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed: small request` | `13.75 us` |
| `http1 parser reader-fed: 1 KiB body` | `27.79 us` |
| `http1 parser reader-fed: 100 KiB body` | `11.35 ms` |
| `http1 parser reader-fed: 1 MiB body` | `175.51 ms` |
| `http1 parser reader-fed: many headers` | `238.73 us` |

## Current: Direct Slice Entry Point

These are the current means for `Http1.Request.parse_slice`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory slice: small request` | `7.67 us` |
| `http1 parser in-memory slice: 1 KiB body` | `12.29 us` |
| `http1 parser in-memory slice: 100 KiB body` | `16.80 us` |
| `http1 parser in-memory slice: 1 MiB body` | `203.80 us` |
| `http1 parser in-memory slice: many headers` | `210.97 us` |

### Reader-Fed

The direct reader-fed slice path currently reads into `Std.IO.IoBuffer` with vectored reads and
then parses `Std.IO.IoBuffer.readable`.

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed slice: small request` | `11.11 us` |
| `http1 parser reader-fed slice: 1 KiB body` | `28.85 us` |
| `http1 parser reader-fed slice: 100 KiB body` | `10.61 ms` |
| `http1 parser reader-fed slice: 1 MiB body` | `174.78 ms` |
| `http1 parser reader-fed slice: many headers` | `262.99 us` |

## Current Read

- Large-body request parsing still benefits clearly from the off-heap slice path in the parser-only
  benchmark. `parse_slice` is materially faster than the old baseline on `100 KiB` and `1 MiB`
  shapes.
- The current reader-fed benchmark regressed versus the earlier `StringView` experiment. Both the
  public parser and the direct slice parser are now around `175 ms` on the `1 MiB` shape, which
  points back at the ingestion/buffer handoff path rather than request-line parsing.
- Tiny requests remain mixed: the slice path is competitive, but setup overhead still dominates the
  small-request and many-header shapes.
- The next optimization target is the reader-fed path: `IoBuffer` fill/consume behavior, repeated
  readable-slice handoff, and any remaining copying or scalar hot loops in the parser stack.
