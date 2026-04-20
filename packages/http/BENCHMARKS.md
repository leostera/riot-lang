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
  - `borrowed`: `Http1.Request.parse_slices`

## Current Summary

### Parser Only

| Shape | Baseline String | Current `parse` | `parse_slice` | `parse_slices` |
| --- | ---: | ---: | ---: | ---: |
| `small request` | `3.14 us` | `8.45 us` | `8.37 us` | `6.17 us` |
| `1 KiB body` | `10.85 us` | `12.99 us` | `12.98 us` | `7.60 us` |
| `100 KiB body` | `174.55 us` | `16.35 us` | `10.70 us` | `7.33 us` |
| `1 MiB body` | `890.20 us` | `84.67 us` | `12.00 us` | `8.60 us` |
| `10 MiB body` | `6.24 ms` | `308.80 us` | `12.60 us` | `8.40 us` |
| `many headers` | `98.42 us` | `189.28 us` | `186.50 us` | `173.29 us` |
| `github navigation request` | `75.30 us` | `173.05 us` | `170.94 us` | `155.18 us` |

### Reader-Driven Full Request

| Shape | Baseline String | Current `parse` | `parse_slice` | `parse_slices` |
| --- | ---: | ---: | ---: | ---: |
| `small request` | `4.51 us` | `9.38 us` | `10.02 us` | `6.21 us` |
| `1 KiB body` | `19.51 us` | `20.59 us` | `19.45 us` | `17.80 us` |
| `100 KiB body` | `508.38 us` | `273.28 us` | `169.70 us` | `193.27 us` |
| `1 MiB body` | `2.15 ms` | `1.48 ms` | `633.47 us` | `687.20 us` |
| `10 MiB body` | `23.71 ms` | `15.04 ms` | `2.79 ms` | `2.38 ms` |
| `many headers` | `106.58 us` | `205.45 us` | `205.28 us` | `189.36 us` |
| `github navigation request` | `97.85 us` | `185.00 us` | `179.38 us` | `169.13 us` |

## Current Read

- Parser-only now cleanly measures just parser cost. Input construction is outside the timed
  closure for all four paths.
- The old string-native parser is still the best parser-only path for tiny and header-heavy
  requests. That is expected: it works directly on the original string and has very little setup.
- For body-heavy parser-only work, the slice and borrowed parsers are decisively better because they
  do not pay a whole-input `string -> IoSlice` adapter cost and they keep the body lazy:
  - `1 MiB`: baseline `890.20 us`, current `84.67 us`, slice `12.00 us`, borrowed `8.60 us`
  - `10 MiB`: baseline `6.24 ms`, current `308.80 us`, slice `12.60 us`, borrowed `8.40 us`
- The reader-driven suite now measures what you asked for: a real `Reader` feeding the whole
  request before parsing. It is explicitly an accumulation-plus-parse benchmark, not a pure parser
  benchmark.
- On medium and large bodies, the slice-based paths clearly beat the old string-native baseline.
  The direct slice path is best at `100 KiB` and `1 MiB`, and borrowed pulls ahead again at
  `10 MiB`:
  - `100 KiB`: baseline `508.38 us`, current `273.28 us`, slice `169.70 us`, borrowed `193.27 us`
  - `1 MiB`: baseline `2.15 ms`, current `1.48 ms`, slice `633.47 us`, borrowed `687.20 us`
  - `10 MiB`: baseline `23.71 ms`, current `15.04 ms`, slice `2.79 ms`, borrowed `2.38 ms`
- The old string-native parser is still hard to beat on tiny and header-heavy shapes, where
  `String` operations are cheap and the ownership advantages of slices do not buy much:
  - `small request`: baseline `4.51 us` vs current `9.38 us`, slice `10.02 us`
  - `github navigation request`: baseline `97.85 us` vs current `185.00 us`, slice `179.38 us`

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
| `http1 parser in-memory baseline string: small request` | `3.14 us` |
| `http1 parser in-memory baseline string: 1 KiB body` | `10.85 us` |
| `http1 parser in-memory baseline string: 100 KiB body` | `174.55 us` |
| `http1 parser in-memory baseline string: 1 MiB body` | `890.20 us` |
| `http1 parser in-memory baseline string: 10 MiB body` | `6.24 ms` |
| `http1 parser in-memory baseline string: many headers` | `98.42 us` |
| `http1 parser in-memory baseline string: github navigation request` | `75.30 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven baseline string: small request` | `4.51 us` |
| `http1 parser reader-driven baseline string: 1 KiB body` | `19.51 us` |
| `http1 parser reader-driven baseline string: 100 KiB body` | `508.38 us` |
| `http1 parser reader-driven baseline string: 1 MiB body` | `2.15 ms` |
| `http1 parser reader-driven baseline string: 10 MiB body` | `23.71 ms` |
| `http1 parser reader-driven baseline string: many headers` | `106.58 us` |
| `http1 parser reader-driven baseline string: github navigation request` | `97.85 us` |

## Current: Public String Entry Point

These are the current means for `Http1.Request.parse`, which converts the whole input string into
an `IoSlice` and then uses the slice parser.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory: small request` | `8.45 us` |
| `http1 parser in-memory: 1 KiB body` | `12.99 us` |
| `http1 parser in-memory: 100 KiB body` | `16.35 us` |
| `http1 parser in-memory: 1 MiB body` | `84.67 us` |
| `http1 parser in-memory: 10 MiB body` | `308.80 us` |
| `http1 parser in-memory: many headers` | `189.28 us` |
| `http1 parser in-memory: github navigation request` | `173.05 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven: small request` | `9.38 us` |
| `http1 parser reader-driven: 1 KiB body` | `20.59 us` |
| `http1 parser reader-driven: 100 KiB body` | `273.28 us` |
| `http1 parser reader-driven: 1 MiB body` | `1.48 ms` |
| `http1 parser reader-driven: 10 MiB body` | `15.04 ms` |
| `http1 parser reader-driven: many headers` | `205.45 us` |
| `http1 parser reader-driven: github navigation request` | `185.00 us` |

## Current: Direct Slice Entry Point

These are the current means for `Http1.Request.parse_slice`. This path materializes the request
head into an owned `Request.t` while keeping the body lazy on `Std.Net.Http.Body.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory slice: small request` | `8.37 us` |
| `http1 parser in-memory slice: 1 KiB body` | `12.98 us` |
| `http1 parser in-memory slice: 100 KiB body` | `10.70 us` |
| `http1 parser in-memory slice: 1 MiB body` | `12.00 us` |
| `http1 parser in-memory slice: 10 MiB body` | `12.60 us` |
| `http1 parser in-memory slice: many headers` | `186.50 us` |
| `http1 parser in-memory slice: github navigation request` | `170.94 us` |

### Reader-Driven Full Request

The direct reader-driven slice path reads the whole payload incrementally into a caller-owned
`Std.IO.IoBuffer`, then parses `Std.IO.IoBuffer.readable`.

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven slice: small request` | `10.02 us` |
| `http1 parser reader-driven slice: 1 KiB body` | `19.45 us` |
| `http1 parser reader-driven slice: 100 KiB body` | `169.70 us` |
| `http1 parser reader-driven slice: 1 MiB body` | `633.47 us` |
| `http1 parser reader-driven slice: 10 MiB body` | `2.79 ms` |
| `http1 parser reader-driven slice: many headers` | `205.28 us` |
| `http1 parser reader-driven slice: github navigation request` | `179.38 us` |

## Current: Borrowed Slice Entry Point

These are the current means for `Http1.Request.parse_slices`, which keeps method, path, version,
headers, and body as borrowed `IoSlice` values instead of materializing a `Request.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory borrowed slice: small request` | `6.17 us` |
| `http1 parser in-memory borrowed slice: 1 KiB body` | `7.60 us` |
| `http1 parser in-memory borrowed slice: 100 KiB body` | `7.33 us` |
| `http1 parser in-memory borrowed slice: 1 MiB body` | `8.60 us` |
| `http1 parser in-memory borrowed slice: 10 MiB body` | `8.40 us` |
| `http1 parser in-memory borrowed slice: many headers` | `173.29 us` |
| `http1 parser in-memory borrowed slice: github navigation request` | `155.18 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven borrowed slice: small request` | `6.21 us` |
| `http1 parser reader-driven borrowed slice: 1 KiB body` | `17.80 us` |
| `http1 parser reader-driven borrowed slice: 100 KiB body` | `193.27 us` |
| `http1 parser reader-driven borrowed slice: 1 MiB body` | `687.20 us` |
| `http1 parser reader-driven borrowed slice: 10 MiB body` | `2.38 ms` |
| `http1 parser reader-driven borrowed slice: many headers` | `189.36 us` |
| `http1 parser reader-driven borrowed slice: github navigation request` | `169.13 us` |
