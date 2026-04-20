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
| `small request` | `3.07 us` | `11.25 us` | `8.73 us` | `5.36 us` |
| `1 KiB body` | `8.89 us` | `14.17 us` | `11.97 us` | `13.21 us` |
| `100 KiB body` | `173.45 us` | `13.73 us` | `9.93 us` | `8.70 us` |
| `1 MiB body` | `1.03 ms` | `57.93 us` | `10.40 us` | `35.13 us` |
| `10 MiB body` | `6.68 ms` | `302.40 us` | `10.40 us` | `8.80 us` |
| `many headers` | `119.81 us` | `210.13 us` | `189.89 us` | `197.43 us` |
| `github navigation request` | `85.09 us` | `185.25 us` | `184.45 us` | `162.22 us` |

### Reader-Driven Full Request

| Shape | Baseline String | Current `parse` | `parse_slice` | Bench Borrowed |
| --- | ---: | ---: | ---: | ---: |
| `small request` | `4.46 us` | `9.02 us` | `9.42 us` | `6.60 us` |
| `1 KiB body` | `16.76 us` | `21.48 us` | `20.76 us` | `17.17 us` |
| `100 KiB body` | `438.83 us` | `265.18 us` | `192.13 us` | `197.58 us` |
| `1 MiB body` | `2.91 ms` | `1.44 ms` | `855.67 us` | `726.53 us` |
| `10 MiB body` | `19.98 ms` | `13.94 ms` | `2.64 ms` | `2.79 ms` |
| `many headers` | `123.05 us` | `205.18 us` | `218.51 us` | `204.12 us` |
| `github navigation request` | `98.46 us` | `188.10 us` | `183.70 us` | `174.28 us` |

## Current Read

- Parser-only now cleanly measures just parser cost. Input construction is outside the timed
  closure for all four paths.
- The old string-native parser is still the best parser-only path for tiny and header-heavy
  requests. That is expected: it works directly on the original string and has very little setup.
- For body-heavy parser-only work, the slice and borrowed parsers are decisively better because they
  do not pay a whole-input `string -> IoSlice` adapter cost and they keep the body lazy:
  - `1 MiB`: baseline `1.03 ms`, current `57.93 us`, slice `10.40 us`, borrowed `35.13 us`
  - `10 MiB`: baseline `6.68 ms`, current `302.40 us`, slice `10.40 us`, borrowed `8.80 us`
- The reader-driven suite now measures what you asked for: a real `Reader` feeding the whole
  request before parsing. It is explicitly an accumulation-plus-parse benchmark, not a pure parser
  benchmark.
- On medium and large bodies, the slice-based paths clearly beat the old string-native baseline.
  The direct slice path is still best at `100 KiB`, and borrowed pulls ahead once the payload is
  larger:
  - `100 KiB`: baseline `438.83 us`, current `265.18 us`, slice `192.13 us`, borrowed `197.58 us`
  - `1 MiB`: baseline `2.91 ms`, current `1.44 ms`, slice `855.67 us`, borrowed `726.53 us`
  - `10 MiB`: baseline `19.98 ms`, current `13.94 ms`, slice `2.64 ms`, borrowed `2.79 ms`
- The old string-native parser is still hard to beat on tiny and header-heavy shapes, where
  `String` operations are cheap and the ownership advantages of slices do not buy much:
  - `small request`: baseline `4.46 us` vs current `9.02 us`, slice `9.42 us`
  - `github navigation request`: baseline `98.46 us` vs current `188.10 us`, slice `183.70 us`

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
| `http1 parser in-memory baseline string: small request` | `3.07 us` |
| `http1 parser in-memory baseline string: 1 KiB body` | `8.89 us` |
| `http1 parser in-memory baseline string: 100 KiB body` | `173.45 us` |
| `http1 parser in-memory baseline string: 1 MiB body` | `1.03 ms` |
| `http1 parser in-memory baseline string: 10 MiB body` | `6.68 ms` |
| `http1 parser in-memory baseline string: many headers` | `119.81 us` |
| `http1 parser in-memory baseline string: github navigation request` | `85.09 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven baseline string: small request` | `4.46 us` |
| `http1 parser reader-driven baseline string: 1 KiB body` | `16.76 us` |
| `http1 parser reader-driven baseline string: 100 KiB body` | `438.83 us` |
| `http1 parser reader-driven baseline string: 1 MiB body` | `2.91 ms` |
| `http1 parser reader-driven baseline string: 10 MiB body` | `19.98 ms` |
| `http1 parser reader-driven baseline string: many headers` | `123.05 us` |
| `http1 parser reader-driven baseline string: github navigation request` | `98.46 us` |

## Current: Public String Entry Point

These are the current means for `Http1.Request.parse`, which converts the whole input string into
an `IoSlice` and then uses the slice parser.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory: small request` | `11.25 us` |
| `http1 parser in-memory: 1 KiB body` | `14.17 us` |
| `http1 parser in-memory: 100 KiB body` | `13.73 us` |
| `http1 parser in-memory: 1 MiB body` | `57.93 us` |
| `http1 parser in-memory: 10 MiB body` | `302.40 us` |
| `http1 parser in-memory: many headers` | `210.13 us` |
| `http1 parser in-memory: github navigation request` | `185.25 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven: small request` | `9.02 us` |
| `http1 parser reader-driven: 1 KiB body` | `21.48 us` |
| `http1 parser reader-driven: 100 KiB body` | `265.18 us` |
| `http1 parser reader-driven: 1 MiB body` | `1.44 ms` |
| `http1 parser reader-driven: 10 MiB body` | `13.94 ms` |
| `http1 parser reader-driven: many headers` | `205.18 us` |
| `http1 parser reader-driven: github navigation request` | `188.10 us` |

## Current: Direct Slice Entry Point

These are the current means for `Http1.Request.parse_slice`. This path materializes the request
head into an owned `Request.t` while keeping the body lazy on `Std.Net.Http.Body.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory slice: small request` | `8.73 us` |
| `http1 parser in-memory slice: 1 KiB body` | `11.97 us` |
| `http1 parser in-memory slice: 100 KiB body` | `9.93 us` |
| `http1 parser in-memory slice: 1 MiB body` | `10.40 us` |
| `http1 parser in-memory slice: 10 MiB body` | `10.40 us` |
| `http1 parser in-memory slice: many headers` | `189.89 us` |
| `http1 parser in-memory slice: github navigation request` | `184.45 us` |

### Reader-Driven Full Request

The direct reader-driven slice path reads the whole payload incrementally into a caller-owned
`Std.IO.IoBuffer`, then parses `Std.IO.IoBuffer.readable`.

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven slice: small request` | `9.42 us` |
| `http1 parser reader-driven slice: 1 KiB body` | `20.76 us` |
| `http1 parser reader-driven slice: 100 KiB body` | `192.13 us` |
| `http1 parser reader-driven slice: 1 MiB body` | `855.67 us` |
| `http1 parser reader-driven slice: 10 MiB body` | `2.64 ms` |
| `http1 parser reader-driven slice: many headers` | `218.51 us` |
| `http1 parser reader-driven slice: github navigation request` | `183.70 us` |

## Current: Borrowed Slice Entry Point

These are the current means for the bench-local borrowed parser, which keeps method, path,
version, headers, and body as borrowed `IoSlice` values instead of materializing a `Request.t`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory borrowed slice: small request` | `5.36 us` |
| `http1 parser in-memory borrowed slice: 1 KiB body` | `13.21 us` |
| `http1 parser in-memory borrowed slice: 100 KiB body` | `8.70 us` |
| `http1 parser in-memory borrowed slice: 1 MiB body` | `35.13 us` |
| `http1 parser in-memory borrowed slice: 10 MiB body` | `8.80 us` |
| `http1 parser in-memory borrowed slice: many headers` | `197.43 us` |
| `http1 parser in-memory borrowed slice: github navigation request` | `162.22 us` |

### Reader-Driven Full Request

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven borrowed slice: small request` | `6.60 us` |
| `http1 parser reader-driven borrowed slice: 1 KiB body` | `17.17 us` |
| `http1 parser reader-driven borrowed slice: 100 KiB body` | `197.58 us` |
| `http1 parser reader-driven borrowed slice: 1 MiB body` | `726.53 us` |
| `http1 parser reader-driven borrowed slice: 10 MiB body` | `2.79 ms` |
| `http1 parser reader-driven borrowed slice: many headers` | `204.12 us` |
| `http1 parser reader-driven borrowed slice: github navigation request` | `174.28 us` |
