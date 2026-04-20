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
  - `borrowed`: `Http1.Request.Borrowed.parse`

## Current Summary

### Parser Only

| Shape | Baseline String | Current `parse` | `parse_slice` | `Borrowed.parse` |
| --- | ---: | ---: | ---: | ---: |
| `small request` | `3.19 us` | `8.46 us` | `8.14 us` | `6.98 us` |
| `1 KiB body` | `5.00 us` | `12.49 us` | `12.71 us` | `8.09 us` |
| `100 KiB body` | `174.40 us` | `12.93 us` | `10.28 us` | `17.62 us` |
| `1 MiB body` | `834.13 us` | `34.07 us` | `10.13 us` | `8.20 us` |
| `10 MiB body` | `5.04 ms` | `426.80 us` | `10.20 us` | `8.00 us` |
| `many headers` | `117.03 us` | `203.62 us` | `189.95 us` | `171.99 us` |
| `github navigation request` | `82.63 us` | `190.32 us` | `167.92 us` | `158.39 us` |

### Reader-Driven Full Request

| Shape | Baseline String | Current `parse` | `parse_slice` | `Borrowed.parse` |
| --- | ---: | ---: | ---: | ---: |
| `small request` | `4.37 us` | `9.04 us` | `10.18 us` | `5.93 us` |
| `1 KiB body` | `23.97 us` | `21.29 us` | `19.75 us` | `16.79 us` |
| `100 KiB body` | `453.33 us` | `252.92 us` | `147.45 us` | `176.08 us` |
| `1 MiB body` | `2.24 ms` | `1.68 ms` | `641.53 us` | `559.93 us` |
| `10 MiB body` | `14.40 ms` | `10.79 ms` | `2.55 ms` | `1.92 ms` |
| `many headers` | `115.69 us` | `203.88 us` | `202.41 us` | `188.39 us` |
| `github navigation request` | `88.58 us` | `184.18 us` | `179.97 us` | `170.63 us` |

## Current Read

- Parser-only now cleanly measures just parser cost. Input construction is outside the timed
  closure for all four paths.
- The old string-native parser is still the best parser-only path for tiny and header-heavy
  requests. That is expected: it works directly on the original string and has very little setup.
- For body-heavy parser-only work, the slice and borrowed parsers are decisively better because they
  do not pay a whole-input `string -> IoSlice` adapter cost and they keep the body lazy:
  - `1 MiB`: baseline `834.13 us`, current `34.07 us`, slice `10.13 us`, borrowed `8.20 us`
  - `10 MiB`: baseline `5.04 ms`, current `426.80 us`, slice `10.20 us`, borrowed `8.00 us`
- The reader-driven suite now measures what you asked for: a real `Reader` feeding the whole
  request before parsing. It is explicitly an accumulation-plus-parse benchmark, not a pure parser
  benchmark.
- On medium and large bodies, the slice-based paths clearly beat the old string-native baseline.
  The direct slice path is still best at `100 KiB`, and borrowed pulls ahead once the payload is
  larger:
  - `100 KiB`: baseline `453.33 us`, current `252.92 us`, slice `147.45 us`, borrowed `176.08 us`
  - `1 MiB`: baseline `2.24 ms`, current `1.68 ms`, slice `641.53 us`, borrowed `559.93 us`
  - `10 MiB`: baseline `14.40 ms`, current `10.79 ms`, slice `2.55 ms`, borrowed `1.92 ms`
- The old string-native parser is still hard to beat on tiny and header-heavy shapes, where
  `String` operations are cheap and the ownership advantages of slices do not buy much:
  - `small request`: baseline `4.37 us` vs current `9.04 us`, slice `10.18 us`
  - `github navigation request`: baseline `88.58 us` vs current `184.18 us`, slice `179.97 us`

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
| `http1 parser in-memory baseline string: 1 KiB body` | `5.00 us` |
| `http1 parser in-memory baseline string: 100 KiB body` | `174.40 us` |
| `http1 parser in-memory baseline string: 1 MiB body` | `834.13 us` |
| `http1 parser in-memory baseline string: 10 MiB body` | `5.04 ms` |
| `http1 parser in-memory baseline string: many headers` | `117.03 us` |
| `http1 parser in-memory baseline string: github navigation request` | `82.63 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven baseline string: small request` | `4.37 us` |
| `http1 parser reader-driven baseline string: 1 KiB body` | `23.97 us` |
| `http1 parser reader-driven baseline string: 100 KiB body` | `453.33 us` |
| `http1 parser reader-driven baseline string: 1 MiB body` | `2.24 ms` |
| `http1 parser reader-driven baseline string: 10 MiB body` | `14.40 ms` |
| `http1 parser reader-driven baseline string: many headers` | `115.69 us` |
| `http1 parser reader-driven baseline string: github navigation request` | `88.58 us` |

## Current: Public String Entry Point

These are the current means for `Http1.Request.parse`, which converts the whole input string into
an `IoSlice` and then uses the slice parser.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory: small request` | `8.46 us` |
| `http1 parser in-memory: 1 KiB body` | `12.49 us` |
| `http1 parser in-memory: 100 KiB body` | `12.93 us` |
| `http1 parser in-memory: 1 MiB body` | `34.07 us` |
| `http1 parser in-memory: 10 MiB body` | `426.80 us` |
| `http1 parser in-memory: many headers` | `203.62 us` |
| `http1 parser in-memory: github navigation request` | `190.32 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven: small request` | `9.04 us` |
| `http1 parser reader-driven: 1 KiB body` | `21.29 us` |
| `http1 parser reader-driven: 100 KiB body` | `252.92 us` |
| `http1 parser reader-driven: 1 MiB body` | `1.68 ms` |
| `http1 parser reader-driven: 10 MiB body` | `10.79 ms` |
| `http1 parser reader-driven: many headers` | `203.88 us` |
| `http1 parser reader-driven: github navigation request` | `184.18 us` |

## Current: Direct Slice Entry Point

These are the current means for `Http1.Request.parse_slice`. This path materializes the request
head into an owned `Request.t` while keeping the body lazy on `Std.Net.Http.Body.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory slice: small request` | `8.14 us` |
| `http1 parser in-memory slice: 1 KiB body` | `12.71 us` |
| `http1 parser in-memory slice: 100 KiB body` | `10.28 us` |
| `http1 parser in-memory slice: 1 MiB body` | `10.13 us` |
| `http1 parser in-memory slice: 10 MiB body` | `10.20 us` |
| `http1 parser in-memory slice: many headers` | `189.95 us` |
| `http1 parser in-memory slice: github navigation request` | `167.92 us` |

### Reader-Driven Full Request

The direct reader-driven slice path reads the whole payload incrementally into a caller-owned
`Std.IO.IoBuffer`, then parses `Std.IO.IoBuffer.readable`.

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven slice: small request` | `10.18 us` |
| `http1 parser reader-driven slice: 1 KiB body` | `19.75 us` |
| `http1 parser reader-driven slice: 100 KiB body` | `147.45 us` |
| `http1 parser reader-driven slice: 1 MiB body` | `641.53 us` |
| `http1 parser reader-driven slice: 10 MiB body` | `2.55 ms` |
| `http1 parser reader-driven slice: many headers` | `202.41 us` |
| `http1 parser reader-driven slice: github navigation request` | `179.97 us` |

## Current: Borrowed Slice Entry Point

These are the current means for `Http1.Request.Borrowed.parse`, which keeps method, path, version,
headers, and body as borrowed `IoSlice` values instead of materializing a `Request.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory borrowed slice: small request` | `6.98 us` |
| `http1 parser in-memory borrowed slice: 1 KiB body` | `8.09 us` |
| `http1 parser in-memory borrowed slice: 100 KiB body` | `17.62 us` |
| `http1 parser in-memory borrowed slice: 1 MiB body` | `8.20 us` |
| `http1 parser in-memory borrowed slice: 10 MiB body` | `8.00 us` |
| `http1 parser in-memory borrowed slice: many headers` | `171.99 us` |
| `http1 parser in-memory borrowed slice: github navigation request` | `158.39 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven borrowed slice: small request` | `5.93 us` |
| `http1 parser reader-driven borrowed slice: 1 KiB body` | `16.79 us` |
| `http1 parser reader-driven borrowed slice: 100 KiB body` | `176.08 us` |
| `http1 parser reader-driven borrowed slice: 1 MiB body` | `559.93 us` |
| `http1 parser reader-driven borrowed slice: 10 MiB body` | `1.92 ms` |
| `http1 parser reader-driven borrowed slice: many headers` | `188.39 us` |
| `http1 parser reader-driven borrowed slice: github navigation request` | `170.63 us` |
