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
- The reader-fed benchmark now uses a single chunking layer:
  - `Std.String.to_reader` provides the full payload
  - `Std.IO.buffered ~chunk_size` simulates transport chunking
- The string-fed transport paths now exercise `Std.IO.read_all_into_buffer`, so the benchmark is
  going through the explicit caller-owned off-heap accumulation API rather than the older
  compatibility alias.
- The current reader-fed numbers below were refreshed after the additive `Std.IO.Writer`
  buffer-native API landed; that change does not affect these read-path measurements directly, but
  the summary now reflects the current tree exactly.
- `Std.IO.Buffer` is now the default off-heap buffer surface. The string entrypoints accumulate into
  that buffer and materialize only at the final `contents` boundary; the slice and borrowed paths
  still accumulate into a caller-owned, pre-sized `IoBuffer`.
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
| `small request` | `4.70 us` | `10.17 us` | `10.92 us` | `8.10 us` |
| `1 KiB body` | `11.53 us` | `14.52 us` | `13.97 us` | `12.35 us` |
| `100 KiB body` | `263.47 us` | `48.52 us` | `20.35 us` | `40.85 us` |
| `1 MiB body` | `1.49 ms` | `591.60 us` | `246.87 us` | `296.00 us` |
| `10 MiB body` | `5.82 ms` | `3.98 ms` | `1.28 ms` | `1.06 ms` |
| `many headers` | `104.38 us` | `201.04 us` | `197.98 us` | `180.56 us` |
| `github navigation request` | `75.42 us` | `183.58 us` | `178.87 us` | `156.74 us` |

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
- The earlier reader-fed regression was mostly a benchmark artifact:
  - it chunked at two layers (`String.to_reader ~chunk_size` and `IO.buffered`)
  - and it let the slice destination `IoBuffer` grow from zero while the string path pre-sized its
    destination buffer
- With one chunking layer, a caller-owned pre-sized `IoBuffer` for the slice paths, and
  `Std.IO.Buffer` now backed by off-heap storage too, the slice parser wins clearly on medium and
  large reader-fed bodies:
  - `100 KiB`: baseline `263.47 us`, current `48.52 us`, slice `20.35 us`
  - `1 MiB`: baseline `1.49 ms`, current `591.60 us`, slice `246.87 us`
  - `10 MiB`: baseline `5.82 ms`, current `3.98 ms`, slice `1.28 ms`
- The borrowed parser remains the throughput floor for large bodies because it avoids owned request
  construction:
  - `1 MiB`: borrowed `296.00 us`
  - `10 MiB`: borrowed `1.06 ms`
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
| `http1 parser reader-fed baseline string: small request` | `4.70 us` |
| `http1 parser reader-fed baseline string: 1 KiB body` | `11.53 us` |
| `http1 parser reader-fed baseline string: 100 KiB body` | `263.47 us` |
| `http1 parser reader-fed baseline string: 1 MiB body` | `1.49 ms` |
| `http1 parser reader-fed baseline string: 10 MiB body` | `5.82 ms` |
| `http1 parser reader-fed baseline string: many headers` | `104.38 us` |
| `http1 parser reader-fed baseline string: github navigation request` | `75.42 us` |

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
| `http1 parser reader-fed: small request` | `10.17 us` |
| `http1 parser reader-fed: 1 KiB body` | `14.52 us` |
| `http1 parser reader-fed: 100 KiB body` | `48.52 us` |
| `http1 parser reader-fed: 1 MiB body` | `591.60 us` |
| `http1 parser reader-fed: 10 MiB body` | `3.98 ms` |
| `http1 parser reader-fed: many headers` | `201.04 us` |
| `http1 parser reader-fed: github navigation request` | `183.58 us` |

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

The direct reader-fed slice path currently reads into a caller-owned, pre-sized `Std.IO.IoBuffer`
with vectored reads and then parses `Std.IO.IoBuffer.readable`.

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed slice: small request` | `10.92 us` |
| `http1 parser reader-fed slice: 1 KiB body` | `13.97 us` |
| `http1 parser reader-fed slice: 100 KiB body` | `20.35 us` |
| `http1 parser reader-fed slice: 1 MiB body` | `246.87 us` |
| `http1 parser reader-fed slice: 10 MiB body` | `1.28 ms` |
| `http1 parser reader-fed slice: many headers` | `197.98 us` |
| `http1 parser reader-fed slice: github navigation request` | `178.87 us` |

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
| `http1 parser reader-fed borrowed slice: small request` | `8.10 us` |
| `http1 parser reader-fed borrowed slice: 1 KiB body` | `12.35 us` |
| `http1 parser reader-fed borrowed slice: 100 KiB body` | `40.85 us` |
| `http1 parser reader-fed borrowed slice: 1 MiB body` | `296.00 us` |
| `http1 parser reader-fed borrowed slice: 10 MiB body` | `1.06 ms` |
| `http1 parser reader-fed borrowed slice: many headers` | `180.56 us` |
| `http1 parser reader-fed borrowed slice: github navigation request` | `156.74 us` |
