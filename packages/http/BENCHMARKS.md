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
- The benchmark suite now keeps four paths distinct:
  - `baseline string`: the old string-native parser, kept benchmark-only
  - `current parse`: the public `Http1.Request.parse` wrapper, which converts `string -> IoSlice`
    and then uses the slice parser
  - `slice`: `Http1.Request.parse_slice`
  - `borrowed`: `Http1.Request.parse_slices`

## Current Summary

### Parser Only

| Shape | Baseline String | Current `parse` | `parse_slice` | `parse_slices` |
| --- | ---: | ---: | ---: | ---: |
| `small request` | `3.24 us` | `8.48 us` | `7.94 us` | `6.88 us` |
| `1 KiB body` | `12.23 us` | `13.19 us` | `11.77 us` | `7.73 us` |
| `100 KiB body` | `186.22 us` | `13.05 us` | `9.73 us` | `14.65 us` |
| `1 MiB body` | `1.09 ms` | `50.33 us` | `10.47 us` | `9.40 us` |
| `10 MiB body` | `5.77 ms` | `243.40 us` | `10.20 us` | `9.40 us` |
| `many headers` | `108.28 us` | `188.62 us` | `188.21 us` | `177.62 us` |
| `github navigation request` | `73.84 us` | `175.11 us` | `170.13 us` | `160.82 us` |

### Reader-Fed

| Shape | Baseline String | Current `parse` | `parse_slice` | `parse_slices` |
| --- | ---: | ---: | ---: | ---: |
| `small request` | `13.27 us` | `9.28 us` | `15.33 us` | `11.94 us` |
| `1 KiB body` | `16.48 us` | `17.41 us` | `18.75 us` | `19.23 us` |
| `100 KiB body` | `349.80 us` | `105.72 us` | `171.03 us` | `377.08 us` |
| `1 MiB body` | `1.94 ms` | `563.27 us` | `477.07 us` | `762.87 us` |
| `10 MiB body` | `10.64 ms` | `2.74 ms` | `3.99 ms` | `3.10 ms` |
| `many headers` | `442.55 us` | `210.09 us` | `208.32 us` | `205.00 us` |
| `github navigation request` | `85.76 us` | `180.33 us` | `186.58 us` | `188.19 us` |

## Current Read

- The old string-native parser is still the best parser-only path for tiny and header-heavy
  requests. That is expected: it works directly on the original string and has very little setup.
- For body-heavy parser-only work, the slice parser is decisively better. The public wrapper
  `parse` is already much better than the old baseline because lazy `Http.Body.t` removed the eager
  body materialization, but `parse_slice` is still the real winner:
  - `1 MiB`: baseline `1.09 ms`, current `50.33 us`, slice `10.47 us`
  - `10 MiB`: baseline `5.77 ms`, current `243.40 us`, slice `10.20 us`
- The borrowed parser shows the true request-head parsing floor, but it is not always lower than
  `parse_slice`; once the body is lazy, the owned slice parser is already very close to that floor.
- Reader-fed results show something different: parser substrate is no longer the only question.
  `parse` is often faster than `parse_slice` because the current string accumulation path through
  `IO.Buffer` is cheaper than the current `IoBuffer` accumulation path.
- That means the next optimization target is clear:
  - parser-only: the slice parser is already the best real implementation
  - reader-fed: `IoBuffer` ingestion and commit/ensure-free behavior need work before the slice path
    wins end to end
- The sanitized GitHub-like request behaves like the header-heavy case, not the body-heavy case.
  It is dominated by request-line and header handling, so the old string-native parser still looks
  strong there.

## Request Shapes

- `small request`: `GET /health` with `Host` and `Accept` headers, no body
- `1 KiB body`: `POST /v1/data` with `Content-Length: 1024`
- `100 KiB body`: `PUT /bulk` with `Content-Length: 100000`
- `1 MiB body`: `PATCH /archive` with `Content-Length: 1000000`
- `10 MiB body`: `PATCH /archive` with `Content-Length: 10000000`
- `many headers`: `GET /headers` with `Host` plus `80` synthetic headers
- `github navigation request`: sanitized GitHub-like `GET` with a long query string, many browser
  headers, and a large synthetic `Cookie` header

## Baseline String Parser

These are the current means for the old string-native parser, kept benchmark-only in the bench
suite as `BaselineParser`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory baseline string: small request` | `3.24 us` |
| `http1 parser in-memory baseline string: 1 KiB body` | `12.23 us` |
| `http1 parser in-memory baseline string: 100 KiB body` | `186.22 us` |
| `http1 parser in-memory baseline string: 1 MiB body` | `1.09 ms` |
| `http1 parser in-memory baseline string: 10 MiB body` | `5.77 ms` |
| `http1 parser in-memory baseline string: many headers` | `108.28 us` |
| `http1 parser in-memory baseline string: github navigation request` | `73.84 us` |

### Reader-Fed

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed baseline string: small request` | `13.27 us` |
| `http1 parser reader-fed baseline string: 1 KiB body` | `16.48 us` |
| `http1 parser reader-fed baseline string: 100 KiB body` | `349.80 us` |
| `http1 parser reader-fed baseline string: 1 MiB body` | `1.94 ms` |
| `http1 parser reader-fed baseline string: 10 MiB body` | `10.64 ms` |
| `http1 parser reader-fed baseline string: many headers` | `442.55 us` |
| `http1 parser reader-fed baseline string: github navigation request` | `85.76 us` |

## Current: Public String Entry Point

These are the current means for `Http1.Request.parse`, which converts the whole input string into
an `IoSlice` and then uses the slice parser.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory: small request` | `8.48 us` |
| `http1 parser in-memory: 1 KiB body` | `13.19 us` |
| `http1 parser in-memory: 100 KiB body` | `13.05 us` |
| `http1 parser in-memory: 1 MiB body` | `50.33 us` |
| `http1 parser in-memory: 10 MiB body` | `243.40 us` |
| `http1 parser in-memory: many headers` | `188.62 us` |
| `http1 parser in-memory: github navigation request` | `175.11 us` |

### Reader-Fed

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed: small request` | `9.28 us` |
| `http1 parser reader-fed: 1 KiB body` | `17.41 us` |
| `http1 parser reader-fed: 100 KiB body` | `105.72 us` |
| `http1 parser reader-fed: 1 MiB body` | `563.27 us` |
| `http1 parser reader-fed: 10 MiB body` | `2.74 ms` |
| `http1 parser reader-fed: many headers` | `210.09 us` |
| `http1 parser reader-fed: github navigation request` | `180.33 us` |

## Current: Direct Slice Entry Point

These are the current means for `Http1.Request.parse_slice`. This path materializes the request
head into an owned `Request.t` while keeping the body lazy on `Std.Net.Http.Body.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory slice: small request` | `7.94 us` |
| `http1 parser in-memory slice: 1 KiB body` | `11.77 us` |
| `http1 parser in-memory slice: 100 KiB body` | `9.73 us` |
| `http1 parser in-memory slice: 1 MiB body` | `10.47 us` |
| `http1 parser in-memory slice: 10 MiB body` | `10.20 us` |
| `http1 parser in-memory slice: many headers` | `188.21 us` |
| `http1 parser in-memory slice: github navigation request` | `170.13 us` |

### Reader-Fed

The direct reader-fed slice path currently reads into `Std.IO.IoBuffer` with vectored reads and
then parses `Std.IO.IoBuffer.readable`.

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed slice: small request` | `15.33 us` |
| `http1 parser reader-fed slice: 1 KiB body` | `18.75 us` |
| `http1 parser reader-fed slice: 100 KiB body` | `171.03 us` |
| `http1 parser reader-fed slice: 1 MiB body` | `477.07 us` |
| `http1 parser reader-fed slice: 10 MiB body` | `3.99 ms` |
| `http1 parser reader-fed slice: many headers` | `208.32 us` |
| `http1 parser reader-fed slice: github navigation request` | `186.58 us` |

## Current: Borrowed Slice Entry Point

These are the current means for `Http1.Request.parse_slices`, which keeps method, path, version,
headers, and body as borrowed `IoSlice` values instead of materializing a `Request.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory borrowed slice: small request` | `6.88 us` |
| `http1 parser in-memory borrowed slice: 1 KiB body` | `7.73 us` |
| `http1 parser in-memory borrowed slice: 100 KiB body` | `14.65 us` |
| `http1 parser in-memory borrowed slice: 1 MiB body` | `9.40 us` |
| `http1 parser in-memory borrowed slice: 10 MiB body` | `9.40 us` |
| `http1 parser in-memory borrowed slice: many headers` | `177.62 us` |
| `http1 parser in-memory borrowed slice: github navigation request` | `160.82 us` |

### Reader-Fed

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed borrowed slice: small request` | `11.94 us` |
| `http1 parser reader-fed borrowed slice: 1 KiB body` | `19.23 us` |
| `http1 parser reader-fed borrowed slice: 100 KiB body` | `377.08 us` |
| `http1 parser reader-fed borrowed slice: 1 MiB body` | `762.87 us` |
| `http1 parser reader-fed borrowed slice: 10 MiB body` | `3.10 ms` |
| `http1 parser reader-fed borrowed slice: many headers` | `205.00 us` |
| `http1 parser reader-fed borrowed slice: github navigation request` | `188.19 us` |
