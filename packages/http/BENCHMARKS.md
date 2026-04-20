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
- `http1_parser_transport_bench` now measures true reader-driven head parsing.
  - each path reads incrementally from `Std.String.to_reader ~chunk_size`
  - the parser is retried as bytes arrive
  - the benchmark stops as soon as the request head is complete
  - the body is not eagerly read to EOF first
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

### Reader-Driven Head Parse

| Shape | Baseline String | Current `parse` | `parse_slice` | `parse_slices` |
| --- | ---: | ---: | ---: | ---: |
| `small request` | `5.00 us` | `13.25 us` | `12.03 us` | `10.10 us` |
| `1 KiB body` | `11.27 us` | `20.09 us` | `17.53 us` | `16.49 us` |
| `100 KiB body` | `4.92 us` | `10.48 us` | `20.23 us` | `9.03 us` |
| `1 MiB body` | `47.27 us` | `14.13 us` | `13.73 us` | `9.67 us` |
| `10 MiB body` | `9.60 us` | `12.60 us` | `14.20 us` | `10.40 us` |
| `many headers` | `1.37 ms` | `2.58 ms` | `2.56 ms` | `2.53 ms` |
| `github navigation request` | `1.05 ms` | `2.35 ms` | `2.26 ms` | `2.25 ms` |

## Current Read

- Parser-only now cleanly measures just parser cost. Input construction is outside the timed
  closure for all four paths.
- The old string-native parser is still the best parser-only path for tiny and header-heavy
  requests. That is expected: it works directly on the original string and has very little setup.
- For body-heavy parser-only work, the slice and borrowed parsers are decisively better because they
  do not pay a whole-input `string -> IoSlice` adapter cost and they keep the body lazy:
  - `1 MiB`: baseline `1.17 ms`, current `57.80 us`, slice `10.60 us`, borrowed `7.73 us`
  - `10 MiB`: baseline `4.44 ms`, current `484.60 us`, slice `10.60 us`, borrowed `8.20 us`
- The reader-driven suite now measures what an actual server head parser cares about:
  - bytes arrive through a `Reader`
  - the parser is retried incrementally
  - parsing stops once the head is complete
  - the body is left unread
- Because the body is no longer read to EOF first, the large-body rows are now roughly flat. They
  are best read as “how expensive is it to reach the end of the head under this transport chunk
  size,” not as bulk-ingestion timings.
- The baseline string path is still strong when the head arrives in one or two reads, but it scales
  poorly for header-heavy streaming because it must repeatedly materialize `StringBuilder.contents`
  and restart the parser from the beginning on every refill:
  - `many headers`: baseline `1.37 ms`, current `2.58 ms`, slice `2.56 ms`, borrowed `2.53 ms`
  - `github navigation request`: baseline `1.05 ms`, current `2.35 ms`, slice `2.26 ms`,
    borrowed `2.25 ms`
- The slice and borrowed paths now show the real low-level advantage more honestly:
  - they no longer pay whole-body accumulation before parsing
  - borrowed mainly saves result materialization; all four paths still reparse from the beginning
    on each refill because the current request parser surface is stateless

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

### Reader-Driven Head Parse

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven baseline string: small request` | `5.00 us` |
| `http1 parser reader-driven baseline string: 1 KiB body` | `11.27 us` |
| `http1 parser reader-driven baseline string: 100 KiB body` | `4.92 us` |
| `http1 parser reader-driven baseline string: 1 MiB body` | `47.27 us` |
| `http1 parser reader-driven baseline string: 10 MiB body` | `9.60 us` |
| `http1 parser reader-driven baseline string: many headers` | `1.37 ms` |
| `http1 parser reader-driven baseline string: github navigation request` | `1.05 ms` |

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

### Reader-Driven Head Parse

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven: small request` | `13.25 us` |
| `http1 parser reader-driven: 1 KiB body` | `20.09 us` |
| `http1 parser reader-driven: 100 KiB body` | `10.48 us` |
| `http1 parser reader-driven: 1 MiB body` | `14.13 us` |
| `http1 parser reader-driven: 10 MiB body` | `12.60 us` |
| `http1 parser reader-driven: many headers` | `2.58 ms` |
| `http1 parser reader-driven: github navigation request` | `2.35 ms` |

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

### Reader-Driven Head Parse

The direct reader-driven slice path reads incrementally into a caller-owned `Std.IO.IoBuffer` with
vectored reads and retries parsing after each refill.

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven slice: small request` | `12.03 us` |
| `http1 parser reader-driven slice: 1 KiB body` | `17.53 us` |
| `http1 parser reader-driven slice: 100 KiB body` | `20.23 us` |
| `http1 parser reader-driven slice: 1 MiB body` | `13.73 us` |
| `http1 parser reader-driven slice: 10 MiB body` | `14.20 us` |
| `http1 parser reader-driven slice: many headers` | `2.56 ms` |
| `http1 parser reader-driven slice: github navigation request` | `2.26 ms` |

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

### Reader-Driven Head Parse

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-driven borrowed slice: small request` | `10.10 us` |
| `http1 parser reader-driven borrowed slice: 1 KiB body` | `16.49 us` |
| `http1 parser reader-driven borrowed slice: 100 KiB body` | `9.03 us` |
| `http1 parser reader-driven borrowed slice: 1 MiB body` | `9.67 us` |
| `http1 parser reader-driven borrowed slice: 10 MiB body` | `10.40 us` |
| `http1 parser reader-driven borrowed slice: many headers` | `2.53 ms` |
| `http1 parser reader-driven borrowed slice: github navigation request` | `2.25 ms` |
