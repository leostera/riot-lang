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
| `small request` | `3.45 us` | `6.93 us` | `9.93 us` | `5.37 us` |
| `1 KiB body` | `8.64 us` | `10.31 us` | `12.66 us` | `10.66 us` |
| `100 KiB body` | `157.28 us` | `25.77 us` | `10.28 us` | `8.55 us` |
| `1 MiB body` | `873.87 us` | `60.53 us` | `10.40 us` | `8.73 us` |
| `10 MiB body` | `5.71 ms` | `230.00 us` | `10.40 us` | `8.60 us` |
| `many headers` | `121.52 us` | `198.36 us` | `192.62 us` | `204.25 us` |
| `github navigation request` | `78.93 us` | `179.26 us` | `180.54 us` | `167.79 us` |

### Reader-Driven Full Request

| Shape | Baseline String | Current `parse` | `parse_slice` | Bench Borrowed |
| --- | ---: | ---: | ---: | ---: |
| `small request` | `4.39 us` | `22.96 us` | `9.25 us` | `7.85 us` |
| `1 KiB body` | `20.83 us` | `19.15 us` | `20.94 us` | `17.12 us` |
| `100 KiB body` | `460.98 us` | `280.45 us` | `197.95 us` | `231.47 us` |
| `1 MiB body` | `2.47 ms` | `1.45 ms` | `762.60 us` | `646.40 us` |
| `10 MiB body` | `22.32 ms` | `13.59 ms` | `2.72 ms` | `2.25 ms` |
| `many headers` | `126.01 us` | `202.24 us` | `205.93 us` | `194.01 us` |
| `github navigation request` | `92.01 us` | `181.53 us` | `187.43 us` | `213.73 us` |

## Current Read

- Parser-only now cleanly measures just parser cost. Input construction is outside the timed
  closure for all four paths.
- The old string-native parser is still the best parser-only path for tiny and header-heavy
  requests. That is expected: it works directly on the original string and has very little setup.
- For body-heavy parser-only work, the slice and borrowed parsers are decisively better because they
  do not pay a whole-input `string -> IoSlice` adapter cost and they keep the body lazy:
  - `1 MiB`: baseline `873.87 us`, current `60.53 us`, slice `10.40 us`, borrowed `8.73 us`
  - `10 MiB`: baseline `5.71 ms`, current `230.00 us`, slice `10.40 us`, borrowed `8.60 us`
- The reader-driven suite now measures what you asked for: a real `Reader` feeding the whole
  request before parsing. It is explicitly an accumulation-plus-parse benchmark, not a pure parser
  benchmark.
- On medium and large bodies, the slice-based paths clearly beat the old string-native baseline.
  The direct slice path is still best at `100 KiB`, and borrowed pulls ahead once the payload is
  larger:
  - `100 KiB`: baseline `460.98 us`, current `280.45 us`, slice `197.95 us`, borrowed `231.47 us`
  - `1 MiB`: baseline `2.47 ms`, current `1.45 ms`, slice `762.60 us`, borrowed `646.40 us`
  - `10 MiB`: baseline `22.32 ms`, current `13.59 ms`, slice `2.72 ms`, borrowed `2.25 ms`
- The old string-native parser is still hard to beat on tiny and header-heavy shapes, where
  `String` operations are cheap and the ownership advantages of slices do not buy much:
  - `small request`: baseline `4.39 us` vs current `22.96 us`, slice `9.25 us`
  - `github navigation request`: baseline `92.01 us` vs current `181.53 us`, slice `187.43 us`

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
| `http1 parser in-memory baseline string: small request` | `3.45 us` |
| `http1 parser in-memory baseline string: 1 KiB body` | `8.64 us` |
| `http1 parser in-memory baseline string: 100 KiB body` | `157.28 us` |
| `http1 parser in-memory baseline string: 1 MiB body` | `873.87 us` |
| `http1 parser in-memory baseline string: 10 MiB body` | `5.71 ms` |
| `http1 parser in-memory baseline string: many headers` | `121.52 us` |
| `http1 parser in-memory baseline string: github navigation request` | `78.93 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven baseline string: small request` | `4.39 us` |
| `http1 parser reader-driven baseline string: 1 KiB body` | `20.83 us` |
| `http1 parser reader-driven baseline string: 100 KiB body` | `460.98 us` |
| `http1 parser reader-driven baseline string: 1 MiB body` | `2.47 ms` |
| `http1 parser reader-driven baseline string: 10 MiB body` | `22.32 ms` |
| `http1 parser reader-driven baseline string: many headers` | `126.01 us` |
| `http1 parser reader-driven baseline string: github navigation request` | `92.01 us` |

## Current: Public String Entry Point

These are the current means for `Http1.Request.parse`, which converts the whole input string into
an `IoSlice` and then uses the slice parser.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory: small request` | `6.93 us` |
| `http1 parser in-memory: 1 KiB body` | `10.31 us` |
| `http1 parser in-memory: 100 KiB body` | `25.77 us` |
| `http1 parser in-memory: 1 MiB body` | `60.53 us` |
| `http1 parser in-memory: 10 MiB body` | `230.00 us` |
| `http1 parser in-memory: many headers` | `198.36 us` |
| `http1 parser in-memory: github navigation request` | `179.26 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven: small request` | `22.96 us` |
| `http1 parser reader-driven: 1 KiB body` | `19.15 us` |
| `http1 parser reader-driven: 100 KiB body` | `280.45 us` |
| `http1 parser reader-driven: 1 MiB body` | `1.45 ms` |
| `http1 parser reader-driven: 10 MiB body` | `13.59 ms` |
| `http1 parser reader-driven: many headers` | `202.24 us` |
| `http1 parser reader-driven: github navigation request` | `181.53 us` |

## Current: Direct Slice Entry Point

These are the current means for `Http1.Request.parse_slice`. This path materializes the request
head into an owned `Request.t` while keeping the body lazy on `Std.Net.Http.Body.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory slice: small request` | `9.93 us` |
| `http1 parser in-memory slice: 1 KiB body` | `12.66 us` |
| `http1 parser in-memory slice: 100 KiB body` | `10.28 us` |
| `http1 parser in-memory slice: 1 MiB body` | `10.40 us` |
| `http1 parser in-memory slice: 10 MiB body` | `10.40 us` |
| `http1 parser in-memory slice: many headers` | `192.62 us` |
| `http1 parser in-memory slice: github navigation request` | `180.54 us` |

### Reader-Driven Full Request

The direct reader-driven slice path reads the whole payload incrementally into a caller-owned
`Std.IO.IoBuffer`, then parses `Std.IO.IoBuffer.readable`.

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven slice: small request` | `9.25 us` |
| `http1 parser reader-driven slice: 1 KiB body` | `20.94 us` |
| `http1 parser reader-driven slice: 100 KiB body` | `197.95 us` |
| `http1 parser reader-driven slice: 1 MiB body` | `762.60 us` |
| `http1 parser reader-driven slice: 10 MiB body` | `2.72 ms` |
| `http1 parser reader-driven slice: many headers` | `205.93 us` |
| `http1 parser reader-driven slice: github navigation request` | `187.43 us` |

## Current: Borrowed Slice Entry Point

These are the current means for the bench-local borrowed parser, which keeps method, path,
version, headers, and body as borrowed `IoSlice` values instead of materializing a `Request.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory borrowed slice: small request` | `5.37 us` |
| `http1 parser in-memory borrowed slice: 1 KiB body` | `10.66 us` |
| `http1 parser in-memory borrowed slice: 100 KiB body` | `8.55 us` |
| `http1 parser in-memory borrowed slice: 1 MiB body` | `8.73 us` |
| `http1 parser in-memory borrowed slice: 10 MiB body` | `8.60 us` |
| `http1 parser in-memory borrowed slice: many headers` | `204.25 us` |
| `http1 parser in-memory borrowed slice: github navigation request` | `167.79 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven borrowed slice: small request` | `7.85 us` |
| `http1 parser reader-driven borrowed slice: 1 KiB body` | `17.12 us` |
| `http1 parser reader-driven borrowed slice: 100 KiB body` | `231.47 us` |
| `http1 parser reader-driven borrowed slice: 1 MiB body` | `646.40 us` |
| `http1 parser reader-driven borrowed slice: 10 MiB body` | `2.25 ms` |
| `http1 parser reader-driven borrowed slice: many headers` | `194.01 us` |
| `http1 parser reader-driven borrowed slice: github navigation request` | `213.73 us` |
