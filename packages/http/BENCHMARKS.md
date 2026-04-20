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
- `http1_parser_bench` measures parser-only cost on prebuilt inputs.
  - request strings are built outside the timed closure
  - `IoSlice` inputs are also prefilled outside the timed closure
- `http1_parser_transport_bench` measures full-request reader-driven parsing.
  - each path reads incrementally from `Std.String.to_reader ~chunk_size`
  - the whole request is accumulated through a `Reader`
  - parsing happens only after the full payload has been read
- The reader-driven suite uses fixture-specific transport chunk sizes:
  - `small request`: `32 B`
  - `1 KiB body`: `64 B`
  - `100 KiB body`: `256 B`
  - `1 MiB body`: `1 KiB`
  - `10 MiB body`: `4 KiB`
  - `many headers`: `64 B`
  - `github navigation request`: `128 B`
- The benchmark suite now keeps four paths distinct:
  - `baseline string`: the old string-native parser, kept benchmark-only
  - `current parse`: the public `Http1.Request.parse` wrapper, which converts `string -> IoSlice`
    and then uses the slice parser
  - `slice`: `Http1.Request.parse_slice`
  - `borrowed`: a bench-local borrowed slice parser kept only for comparison

## Current Summary

### Parser Only

| Shape | Baseline String | Current `parse` | `parse_slice` | Bench Borrowed |
| --- | ---: | ---: | ---: | ---: |
| `small request` | `3.19 us` | `8.82 us` | `6.47 us` | `5.12 us` |
| `1 KiB body` | `11.27 us` | `12.02 us` | `12.09 us` | `8.32 us` |
| `100 KiB body` | `172.83 us` | `11.98 us` | `15.28 us` | `15.53 us` |
| `1 MiB body` | `1.01 ms` | `32.47 us` | `11.00 us` | `8.93 us` |
| `10 MiB body` | `5.73 ms` | `245.40 us` | `11.00 us` | `9.40 us` |
| `many headers` | `111.17 us` | `196.92 us` | `202.24 us` | `224.96 us` |
| `github navigation request` | `72.59 us` | `177.08 us` | `174.11 us` | `176.31 us` |

### Reader-Driven Full Request

| Shape | Baseline String | Current `parse` | `parse_slice` | Bench Borrowed |
| --- | ---: | ---: | ---: | ---: |
| `small request` | `4.50 us` | `9.02 us` | `16.73 us` | `8.05 us` |
| `1 KiB body` | `18.88 us` | `20.53 us` | `23.44 us` | `15.10 us` |
| `100 KiB body` | `626.13 us` | `288.32 us` | `165.83 us` | `190.88 us` |
| `1 MiB body` | `2.28 ms` | `1.41 ms` | `635.73 us` | `835.47 us` |
| `10 MiB body` | `21.60 ms` | `12.94 ms` | `2.49 ms` | `2.38 ms` |
| `many headers` | `110.22 us` | `215.47 us` | `207.49 us` | `204.25 us` |
| `github navigation request` | `94.27 us` | `191.86 us` | `188.61 us` | `177.25 us` |

## Current Read

- Parser-only now cleanly measures just parser cost. Input construction is outside the timed
  closure for all four paths.
- The old string-native parser is still the best parser-only path for tiny and header-heavy
  requests. That is expected: it works directly on the original string and has very little setup.
- For body-heavy parser-only work, the slice and borrowed parsers are decisively better because they
  do not pay a whole-input `string -> IoSlice` adapter cost and they keep the body lazy:
  - `1 MiB`: baseline `1.01 ms`, current `32.47 us`, slice `11.00 us`, borrowed `8.93 us`
  - `10 MiB`: baseline `5.73 ms`, current `245.40 us`, slice `11.00 us`, borrowed `9.40 us`
- The reader-driven suite now measures what you asked for: a real `Reader` feeding the whole
  request before parsing. It is explicitly an accumulation-plus-parse benchmark, not a pure parser
  benchmark.
- On medium and large bodies, the slice-based paths still clearly beat the old string-native
  baseline. The exact winner between owned and borrowed slice paths varies run to run, but both
  stay well ahead of the baseline:
  - `100 KiB`: baseline `626.13 us`, current `288.32 us`, slice `165.83 us`, borrowed `190.88 us`
  - `1 MiB`: baseline `2.28 ms`, current `1.41 ms`, slice `635.73 us`, borrowed `835.47 us`
  - `10 MiB`: baseline `21.60 ms`, current `12.94 ms`, slice `2.49 ms`, borrowed `2.38 ms`
- The old string-native parser is still hard to beat on tiny and header-heavy shapes, where
  `String` operations are cheap and the ownership advantages of slices do not buy much:
  - `small request`: baseline `4.50 us` vs current `9.02 us`, slice `16.73 us`
  - `github navigation request`: baseline `94.27 us` vs current `191.86 us`, slice `188.61 us`

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
| `http1 parser in-memory baseline string: small request` | `3.19 us` |
| `http1 parser in-memory baseline string: 1 KiB body` | `11.27 us` |
| `http1 parser in-memory baseline string: 100 KiB body` | `172.83 us` |
| `http1 parser in-memory baseline string: 1 MiB body` | `1.01 ms` |
| `http1 parser in-memory baseline string: 10 MiB body` | `5.73 ms` |
| `http1 parser in-memory baseline string: many headers` | `111.17 us` |
| `http1 parser in-memory baseline string: github navigation request` | `72.59 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven baseline string: small request` | `4.50 us` |
| `http1 parser reader-driven baseline string: 1 KiB body` | `18.88 us` |
| `http1 parser reader-driven baseline string: 100 KiB body` | `626.13 us` |
| `http1 parser reader-driven baseline string: 1 MiB body` | `2.28 ms` |
| `http1 parser reader-driven baseline string: 10 MiB body` | `21.60 ms` |
| `http1 parser reader-driven baseline string: many headers` | `110.22 us` |
| `http1 parser reader-driven baseline string: github navigation request` | `94.27 us` |

## Current: Public String Entry Point

These are the current means for `Http1.Request.parse`, which converts the whole input string into
an `IoSlice` and then uses the slice parser.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory: small request` | `8.82 us` |
| `http1 parser in-memory: 1 KiB body` | `12.02 us` |
| `http1 parser in-memory: 100 KiB body` | `11.98 us` |
| `http1 parser in-memory: 1 MiB body` | `32.47 us` |
| `http1 parser in-memory: 10 MiB body` | `245.40 us` |
| `http1 parser in-memory: many headers` | `196.92 us` |
| `http1 parser in-memory: github navigation request` | `177.08 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven: small request` | `9.02 us` |
| `http1 parser reader-driven: 1 KiB body` | `20.53 us` |
| `http1 parser reader-driven: 100 KiB body` | `288.32 us` |
| `http1 parser reader-driven: 1 MiB body` | `1.41 ms` |
| `http1 parser reader-driven: 10 MiB body` | `12.94 ms` |
| `http1 parser reader-driven: many headers` | `215.47 us` |
| `http1 parser reader-driven: github navigation request` | `191.86 us` |

## Current: Direct Slice Entry Point

These are the current means for `Http1.Request.parse_slice`. This path materializes the request
head into an owned `Request.t` while keeping the body lazy on `Std.Net.Http.Body.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory slice: small request` | `6.47 us` |
| `http1 parser in-memory slice: 1 KiB body` | `12.09 us` |
| `http1 parser in-memory slice: 100 KiB body` | `15.28 us` |
| `http1 parser in-memory slice: 1 MiB body` | `11.00 us` |
| `http1 parser in-memory slice: 10 MiB body` | `11.00 us` |
| `http1 parser in-memory slice: many headers` | `202.24 us` |
| `http1 parser in-memory slice: github navigation request` | `174.11 us` |

### Reader-Driven Full Request

The direct reader-driven slice path reads the whole payload incrementally into a caller-owned
`Std.IO.IoBuffer`, then parses `Std.IO.IoBuffer.readable`.

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven slice: small request` | `16.73 us` |
| `http1 parser reader-driven slice: 1 KiB body` | `23.44 us` |
| `http1 parser reader-driven slice: 100 KiB body` | `165.83 us` |
| `http1 parser reader-driven slice: 1 MiB body` | `635.73 us` |
| `http1 parser reader-driven slice: 10 MiB body` | `2.49 ms` |
| `http1 parser reader-driven slice: many headers` | `207.49 us` |
| `http1 parser reader-driven slice: github navigation request` | `188.61 us` |

## Current: Borrowed Slice Entry Point

These are the current means for the bench-local borrowed parser, which keeps method, path,
version, headers, and body as borrowed `IoSlice` values instead of materializing a `Request.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory borrowed slice: small request` | `5.12 us` |
| `http1 parser in-memory borrowed slice: 1 KiB body` | `8.32 us` |
| `http1 parser in-memory borrowed slice: 100 KiB body` | `15.53 us` |
| `http1 parser in-memory borrowed slice: 1 MiB body` | `8.93 us` |
| `http1 parser in-memory borrowed slice: 10 MiB body` | `9.40 us` |
| `http1 parser in-memory borrowed slice: many headers` | `224.96 us` |
| `http1 parser in-memory borrowed slice: github navigation request` | `176.31 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven borrowed slice: small request` | `8.05 us` |
| `http1 parser reader-driven borrowed slice: 1 KiB body` | `15.10 us` |
| `http1 parser reader-driven borrowed slice: 100 KiB body` | `190.88 us` |
| `http1 parser reader-driven borrowed slice: 1 MiB body` | `835.47 us` |
| `http1 parser reader-driven borrowed slice: 10 MiB body` | `2.38 ms` |
| `http1 parser reader-driven borrowed slice: many headers` | `204.25 us` |
| `http1 parser reader-driven borrowed slice: github navigation request` | `177.25 us` |
