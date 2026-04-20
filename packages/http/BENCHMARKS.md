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
| `small request` | `5.15 us` | `9.53 us` | `10.81 us` | `11.53 us` |
| `1 KiB body` | `12.19 us` | `18.72 us` | `15.18 us` | `12.07 us` |
| `100 KiB body` | `282.07 us` | `36.92 us` | `17.43 us` | `43.85 us` |
| `1 MiB body` | `2.05 ms` | `518.00 us` | `211.20 us` | `197.47 us` |
| `10 MiB body` | `10.06 ms` | `4.42 ms` | `831.60 us` | `831.40 us` |
| `many headers` | `125.41 us` | `220.78 us` | `212.51 us` | `200.31 us` |
| `github navigation request` | `126.90 us` | `177.48 us` | `294.51 us` | `164.76 us` |

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
  - `100 KiB`: baseline `282.07 us`, current `36.92 us`, slice `17.43 us`
  - `1 MiB`: baseline `2.05 ms`, current `518.00 us`, slice `211.20 us`
  - `10 MiB`: baseline `10.06 ms`, current `4.42 ms`, slice `831.60 us`
- The borrowed parser remains the throughput floor for large bodies because it avoids owned request
  construction:
  - `1 MiB`: borrowed `197.47 us`
  - `10 MiB`: borrowed `831.40 us`
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
| `http1 parser reader-fed baseline string: small request` | `5.15 us` |
| `http1 parser reader-fed baseline string: 1 KiB body` | `12.19 us` |
| `http1 parser reader-fed baseline string: 100 KiB body` | `282.07 us` |
| `http1 parser reader-fed baseline string: 1 MiB body` | `2.05 ms` |
| `http1 parser reader-fed baseline string: 10 MiB body` | `10.06 ms` |
| `http1 parser reader-fed baseline string: many headers` | `125.41 us` |
| `http1 parser reader-fed baseline string: github navigation request` | `126.90 us` |

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
| `http1 parser reader-fed: small request` | `9.53 us` |
| `http1 parser reader-fed: 1 KiB body` | `18.72 us` |
| `http1 parser reader-fed: 100 KiB body` | `36.92 us` |
| `http1 parser reader-fed: 1 MiB body` | `518.00 us` |
| `http1 parser reader-fed: 10 MiB body` | `4.42 ms` |
| `http1 parser reader-fed: many headers` | `220.78 us` |
| `http1 parser reader-fed: github navigation request` | `177.48 us` |

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
| `http1 parser reader-fed slice: small request` | `10.81 us` |
| `http1 parser reader-fed slice: 1 KiB body` | `15.18 us` |
| `http1 parser reader-fed slice: 100 KiB body` | `17.43 us` |
| `http1 parser reader-fed slice: 1 MiB body` | `211.20 us` |
| `http1 parser reader-fed slice: 10 MiB body` | `831.60 us` |
| `http1 parser reader-fed slice: many headers` | `212.51 us` |
| `http1 parser reader-fed slice: github navigation request` | `294.51 us` |

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
| `http1 parser reader-fed borrowed slice: small request` | `11.53 us` |
| `http1 parser reader-fed borrowed slice: 1 KiB body` | `12.07 us` |
| `http1 parser reader-fed borrowed slice: 100 KiB body` | `43.85 us` |
| `http1 parser reader-fed borrowed slice: 1 MiB body` | `197.47 us` |
| `http1 parser reader-fed borrowed slice: 10 MiB body` | `831.40 us` |
| `http1 parser reader-fed borrowed slice: many headers` | `200.31 us` |
| `http1 parser reader-fed borrowed slice: github navigation request` | `164.76 us` |
