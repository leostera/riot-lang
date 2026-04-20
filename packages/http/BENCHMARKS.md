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
| `small request` | `3.03 us` | `8.81 us` | `9.42 us` | `6.60 us` |
| `1 KiB body` | `9.11 us` | `14.31 us` | `12.27 us` | `7.88 us` |
| `100 KiB body` | `358.88 us` | `12.95 us` | `9.72 us` | `11.87 us` |
| `1 MiB body` | `1.17 ms` | `57.80 us` | `10.60 us` | `7.73 us` |
| `10 MiB body` | `4.44 ms` | `484.60 us` | `10.60 us` | `8.20 us` |
| `many headers` | `117.58 us` | `226.62 us` | `197.16 us` | `180.73 us` |
| `github navigation request` | `80.11 us` | `186.92 us` | `181.18 us` | `158.78 us` |

### Reader-Driven Full Request

| Shape | Baseline String | Current `parse` | `parse_slice` | `parse_slices` |
| --- | ---: | ---: | ---: | ---: |
| `small request` | `4.12 us` | `7.66 us` | `9.77 us` | `7.75 us` |
| `1 KiB body` | `7.15 us` | `15.71 us` | `18.71 us` | `16.84 us` |
| `100 KiB body` | `378.95 us` | `79.02 us` | `170.77 us` | `179.13 us` |
| `1 MiB body` | `1.27 ms` | `694.00 us` | `476.60 us` | `1.29 ms` |
| `10 MiB body` | `6.54 ms` | `2.36 ms` | `2.81 ms` | `2.09 ms` |
| `many headers` | `122.05 us` | `184.67 us` | `205.62 us` | `192.59 us` |
| `github navigation request` | `76.78 us` | `181.88 us` | `185.43 us` | `172.16 us` |

## Current Read

- Parser-only now cleanly measures just parser cost. Input construction is outside the timed
  closure for all four paths.
- The old string-native parser is still the best parser-only path for tiny and header-heavy
  requests. That is expected: it works directly on the original string and has very little setup.
- For body-heavy parser-only work, the slice and borrowed parsers are decisively better because they
  do not pay a whole-input `string -> IoSlice` adapter cost and they keep the body lazy:
  - `1 MiB`: baseline `1.17 ms`, current `57.80 us`, slice `10.60 us`, borrowed `7.73 us`
  - `10 MiB`: baseline `4.44 ms`, current `484.60 us`, slice `10.60 us`, borrowed `8.20 us`
- The reader-driven suite now measures what you asked for: a real `Reader` feeding the whole
  request before parsing. It is explicitly an accumulation-plus-parse benchmark, not a pure parser
  benchmark.
- On medium and large bodies, the current and slice-based paths clearly beat the old string-native
  baseline because they avoid the old parser’s body-sized string slicing costs:
  - `100 KiB`: baseline `378.95 us`, current `79.02 us`, slice `170.77 us`
  - `1 MiB`: baseline `1.27 ms`, current `694.00 us`, slice `476.60 us`
  - `10 MiB`: baseline `6.54 ms`, current `2.36 ms`, slice `2.81 ms`, borrowed `2.09 ms`
- The borrowed parser is not always the best full-request path because the benchmark still includes
  transport accumulation, and borrowed only saves result ownership conversion at the parse
  boundary.
- The old string-native parser is still hard to beat on tiny and header-heavy shapes, where
  `String` operations are cheap and the ownership advantages of slices do not buy much:
  - `small request`: baseline `4.12 us` vs current `7.66 us`, slice `9.77 us`
  - `github navigation request`: baseline `76.78 us` vs current `181.88 us`, slice `185.43 us`

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
| `http1 parser in-memory baseline string: small request` | `3.03 us` |
| `http1 parser in-memory baseline string: 1 KiB body` | `9.11 us` |
| `http1 parser in-memory baseline string: 100 KiB body` | `358.88 us` |
| `http1 parser in-memory baseline string: 1 MiB body` | `1.17 ms` |
| `http1 parser in-memory baseline string: 10 MiB body` | `4.44 ms` |
| `http1 parser in-memory baseline string: many headers` | `117.58 us` |
| `http1 parser in-memory baseline string: github navigation request` | `80.11 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven baseline string: small request` | `4.12 us` |
| `http1 parser reader-driven baseline string: 1 KiB body` | `7.15 us` |
| `http1 parser reader-driven baseline string: 100 KiB body` | `378.95 us` |
| `http1 parser reader-driven baseline string: 1 MiB body` | `1.27 ms` |
| `http1 parser reader-driven baseline string: 10 MiB body` | `6.54 ms` |
| `http1 parser reader-driven baseline string: many headers` | `122.05 us` |
| `http1 parser reader-driven baseline string: github navigation request` | `76.78 us` |

## Current: Public String Entry Point

These are the current means for `Http1.Request.parse`, which converts the whole input string into
an `IoSlice` and then uses the slice parser.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory: small request` | `8.81 us` |
| `http1 parser in-memory: 1 KiB body` | `14.31 us` |
| `http1 parser in-memory: 100 KiB body` | `12.95 us` |
| `http1 parser in-memory: 1 MiB body` | `57.80 us` |
| `http1 parser in-memory: 10 MiB body` | `484.60 us` |
| `http1 parser in-memory: many headers` | `226.62 us` |
| `http1 parser in-memory: github navigation request` | `186.92 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven: small request` | `7.66 us` |
| `http1 parser reader-driven: 1 KiB body` | `15.71 us` |
| `http1 parser reader-driven: 100 KiB body` | `79.02 us` |
| `http1 parser reader-driven: 1 MiB body` | `694.00 us` |
| `http1 parser reader-driven: 10 MiB body` | `2.36 ms` |
| `http1 parser reader-driven: many headers` | `184.67 us` |
| `http1 parser reader-driven: github navigation request` | `181.88 us` |

## Current: Direct Slice Entry Point

These are the current means for `Http1.Request.parse_slice`. This path materializes the request
head into an owned `Request.t` while keeping the body lazy on `Std.Net.Http.Body.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory slice: small request` | `9.42 us` |
| `http1 parser in-memory slice: 1 KiB body` | `12.27 us` |
| `http1 parser in-memory slice: 100 KiB body` | `9.72 us` |
| `http1 parser in-memory slice: 1 MiB body` | `10.60 us` |
| `http1 parser in-memory slice: 10 MiB body` | `10.60 us` |
| `http1 parser in-memory slice: many headers` | `197.16 us` |
| `http1 parser in-memory slice: github navigation request` | `181.18 us` |

### Reader-Driven Full Request

The direct reader-driven slice path reads the whole payload incrementally into a caller-owned
`Std.IO.IoBuffer`, then parses `Std.IO.IoBuffer.readable`.

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven slice: small request` | `9.77 us` |
| `http1 parser reader-driven slice: 1 KiB body` | `18.71 us` |
| `http1 parser reader-driven slice: 100 KiB body` | `170.77 us` |
| `http1 parser reader-driven slice: 1 MiB body` | `476.60 us` |
| `http1 parser reader-driven slice: 10 MiB body` | `2.81 ms` |
| `http1 parser reader-driven slice: many headers` | `205.62 us` |
| `http1 parser reader-driven slice: github navigation request` | `185.43 us` |

## Current: Borrowed Slice Entry Point

These are the current means for `Http1.Request.parse_slices`, which keeps method, path, version,
headers, and body as borrowed `IoSlice` values instead of materializing a `Request.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory borrowed slice: small request` | `6.60 us` |
| `http1 parser in-memory borrowed slice: 1 KiB body` | `7.88 us` |
| `http1 parser in-memory borrowed slice: 100 KiB body` | `11.87 us` |
| `http1 parser in-memory borrowed slice: 1 MiB body` | `7.73 us` |
| `http1 parser in-memory borrowed slice: 10 MiB body` | `8.20 us` |
| `http1 parser in-memory borrowed slice: many headers` | `180.73 us` |
| `http1 parser in-memory borrowed slice: github navigation request` | `158.78 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven borrowed slice: small request` | `7.75 us` |
| `http1 parser reader-driven borrowed slice: 1 KiB body` | `16.84 us` |
| `http1 parser reader-driven borrowed slice: 100 KiB body` | `179.13 us` |
| `http1 parser reader-driven borrowed slice: 1 MiB body` | `1.29 ms` |
| `http1 parser reader-driven borrowed slice: 10 MiB body` | `2.09 ms` |
| `http1 parser reader-driven borrowed slice: many headers` | `192.59 us` |
| `http1 parser reader-driven borrowed slice: github navigation request` | `172.16 us` |
