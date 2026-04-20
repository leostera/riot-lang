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
| `small request` | `3.18 us` | `8.78 us` | `8.21 us` | `6.20 us` |
| `1 KiB body` | `10.65 us` | `13.42 us` | `12.00 us` | `8.03 us` |
| `100 KiB body` | `171.68 us` | `20.07 us` | `9.57 us` | `13.57 us` |
| `1 MiB body` | `1.01 ms` | `103.60 us` | `10.07 us` | `7.80 us` |
| `10 MiB body` | `5.59 ms` | `358.40 us` | `10.40 us` | `7.80 us` |
| `many headers` | `107.59 us` | `186.16 us` | `188.36 us` | `165.75 us` |
| `github navigation request` | `73.06 us` | `169.99 us` | `172.30 us` | `158.86 us` |

### Reader-Driven Full Request

| Shape | Baseline String | Current `parse` | `parse_slice` | `parse_slices` |
| --- | ---: | ---: | ---: | ---: |
| `small request` | `4.48 us` | `10.46 us` | `9.39 us` | `7.53 us` |
| `1 KiB body` | `19.98 us` | `19.68 us` | `20.33 us` | `17.06 us` |
| `100 KiB body` | `476.63 us` | `271.20 us` | `159.47 us` | `194.73 us` |
| `1 MiB body` | `2.36 ms` | `1.39 ms` | `891.87 us` | `582.87 us` |
| `10 MiB body` | `14.80 ms` | `11.05 ms` | `2.49 ms` | `1.88 ms` |
| `many headers` | `113.68 us` | `207.38 us` | `210.38 us` | `185.55 us` |
| `github navigation request` | `96.53 us` | `184.05 us` | `180.21 us` | `170.89 us` |

## Current Read

- Parser-only now cleanly measures just parser cost. Input construction is outside the timed
  closure for all four paths.
- The old string-native parser is still the best parser-only path for tiny and header-heavy
  requests. That is expected: it works directly on the original string and has very little setup.
- For body-heavy parser-only work, the slice and borrowed parsers are decisively better because they
  do not pay a whole-input `string -> IoSlice` adapter cost and they keep the body lazy:
  - `1 MiB`: baseline `1.01 ms`, current `103.60 us`, slice `10.07 us`, borrowed `7.80 us`
  - `10 MiB`: baseline `5.59 ms`, current `358.40 us`, slice `10.40 us`, borrowed `7.80 us`
- The reader-driven suite now measures what you asked for: a real `Reader` feeding the whole
  request before parsing. It is explicitly an accumulation-plus-parse benchmark, not a pure parser
  benchmark.
- On medium and large bodies, the slice-based paths clearly beat the old string-native baseline.
  Borrowed is best once the body gets large enough:
  - `100 KiB`: baseline `476.63 us`, current `271.20 us`, slice `159.47 us`, borrowed `194.73 us`
  - `1 MiB`: baseline `2.36 ms`, current `1.39 ms`, slice `891.87 us`, borrowed `582.87 us`
  - `10 MiB`: baseline `14.80 ms`, current `11.05 ms`, slice `2.49 ms`, borrowed `1.88 ms`
- The old string-native parser is still hard to beat on tiny and header-heavy shapes, where
  `String` operations are cheap and the ownership advantages of slices do not buy much:
  - `small request`: baseline `4.48 us` vs current `10.46 us`, slice `9.39 us`
  - `github navigation request`: baseline `96.53 us` vs current `184.05 us`, slice `180.21 us`

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
| `http1 parser in-memory baseline string: small request` | `3.18 us` |
| `http1 parser in-memory baseline string: 1 KiB body` | `10.65 us` |
| `http1 parser in-memory baseline string: 100 KiB body` | `171.68 us` |
| `http1 parser in-memory baseline string: 1 MiB body` | `1.01 ms` |
| `http1 parser in-memory baseline string: 10 MiB body` | `5.59 ms` |
| `http1 parser in-memory baseline string: many headers` | `107.59 us` |
| `http1 parser in-memory baseline string: github navigation request` | `73.06 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven baseline string: small request` | `4.48 us` |
| `http1 parser reader-driven baseline string: 1 KiB body` | `19.98 us` |
| `http1 parser reader-driven baseline string: 100 KiB body` | `476.63 us` |
| `http1 parser reader-driven baseline string: 1 MiB body` | `2.36 ms` |
| `http1 parser reader-driven baseline string: 10 MiB body` | `14.80 ms` |
| `http1 parser reader-driven baseline string: many headers` | `113.68 us` |
| `http1 parser reader-driven baseline string: github navigation request` | `96.53 us` |

## Current: Public String Entry Point

These are the current means for `Http1.Request.parse`, which converts the whole input string into
an `IoSlice` and then uses the slice parser.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory: small request` | `8.78 us` |
| `http1 parser in-memory: 1 KiB body` | `13.42 us` |
| `http1 parser in-memory: 100 KiB body` | `20.07 us` |
| `http1 parser in-memory: 1 MiB body` | `103.60 us` |
| `http1 parser in-memory: 10 MiB body` | `358.40 us` |
| `http1 parser in-memory: many headers` | `186.16 us` |
| `http1 parser in-memory: github navigation request` | `169.99 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven: small request` | `10.46 us` |
| `http1 parser reader-driven: 1 KiB body` | `19.68 us` |
| `http1 parser reader-driven: 100 KiB body` | `271.20 us` |
| `http1 parser reader-driven: 1 MiB body` | `1.39 ms` |
| `http1 parser reader-driven: 10 MiB body` | `11.05 ms` |
| `http1 parser reader-driven: many headers` | `207.38 us` |
| `http1 parser reader-driven: github navigation request` | `184.05 us` |

## Current: Direct Slice Entry Point

These are the current means for `Http1.Request.parse_slice`. This path materializes the request
head into an owned `Request.t` while keeping the body lazy on `Std.Net.Http.Body.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory slice: small request` | `8.21 us` |
| `http1 parser in-memory slice: 1 KiB body` | `12.00 us` |
| `http1 parser in-memory slice: 100 KiB body` | `9.57 us` |
| `http1 parser in-memory slice: 1 MiB body` | `10.07 us` |
| `http1 parser in-memory slice: 10 MiB body` | `10.40 us` |
| `http1 parser in-memory slice: many headers` | `188.36 us` |
| `http1 parser in-memory slice: github navigation request` | `172.30 us` |

### Reader-Driven Full Request

The direct reader-driven slice path reads the whole payload incrementally into a caller-owned
`Std.IO.IoBuffer`, then parses `Std.IO.IoBuffer.readable`.

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven slice: small request` | `9.39 us` |
| `http1 parser reader-driven slice: 1 KiB body` | `20.33 us` |
| `http1 parser reader-driven slice: 100 KiB body` | `159.47 us` |
| `http1 parser reader-driven slice: 1 MiB body` | `891.87 us` |
| `http1 parser reader-driven slice: 10 MiB body` | `2.49 ms` |
| `http1 parser reader-driven slice: many headers` | `210.38 us` |
| `http1 parser reader-driven slice: github navigation request` | `180.21 us` |

## Current: Borrowed Slice Entry Point

These are the current means for `Http1.Request.parse_slices`, which keeps method, path, version,
headers, and body as borrowed `IoSlice` values instead of materializing a `Request.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory borrowed slice: small request` | `6.20 us` |
| `http1 parser in-memory borrowed slice: 1 KiB body` | `8.03 us` |
| `http1 parser in-memory borrowed slice: 100 KiB body` | `13.57 us` |
| `http1 parser in-memory borrowed slice: 1 MiB body` | `7.80 us` |
| `http1 parser in-memory borrowed slice: 10 MiB body` | `7.80 us` |
| `http1 parser in-memory borrowed slice: many headers` | `165.75 us` |
| `http1 parser in-memory borrowed slice: github navigation request` | `158.86 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven borrowed slice: small request` | `7.53 us` |
| `http1 parser reader-driven borrowed slice: 1 KiB body` | `17.06 us` |
| `http1 parser reader-driven borrowed slice: 100 KiB body` | `194.73 us` |
| `http1 parser reader-driven borrowed slice: 1 MiB body` | `582.87 us` |
| `http1 parser reader-driven borrowed slice: 10 MiB body` | `1.88 ms` |
| `http1 parser reader-driven borrowed slice: many headers` | `185.55 us` |
| `http1 parser reader-driven borrowed slice: github navigation request` | `170.89 us` |
