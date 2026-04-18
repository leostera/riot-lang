# HTTP Benchmarks

This file tracks the HTTP/1 request-parser migration from heap-string slicing to
`Std.IO.StringView`.

Date:
- 2026-04-18

Commands:

```sh
timeout 120 riot bench http:http1_parser_bench --json
timeout 120 riot bench http:http1_parser_transport_bench --json
```

Notes:
- `http1_parser_bench` measures parser-only cost on already-materialized request strings.
- `http1_parser_transport_bench` measures `Std.IO` reader accumulation plus parsing.
- The reader-fed benchmark currently uses `Std.String.to_reader` with fixed chunk sizes instead of a
  live socket or server, so it isolates request ingestion overhead without process-management noise.

## Request Shapes

- `small request`: `GET /health` with `Host` and `Accept` headers, no body
- `1 KiB body`: `POST /v1/data` with `Content-Length: 1024`
- `100 KiB body`: `PUT /bulk` with `Content-Length: 100000`
- `1 MiB body`: `PATCH /archive` with `Content-Length: 1000000`
- `many headers`: `GET /headers` with `Host` plus `80` synthetic headers

## Baseline: Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory: small request` | `3.46 us` |
| `http1 parser in-memory: 1 KiB body` | `12.30 us` |
| `http1 parser in-memory: 100 KiB body` | `751.92 us` |
| `http1 parser in-memory: 1 MiB body` | `3.67 ms` |
| `http1 parser in-memory: many headers` | `157.77 us` |

## Baseline: Reader-Fed

Chunk sizes:
- small request: `32`
- `1 KiB` body: `64`
- `100 KiB` body: `256`
- `1 MiB` body: `1024`
- many headers: `64`

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed: small request` | `9.82 us` |
| `http1 parser reader-fed: 1 KiB body` | `17.42 us` |
| `http1 parser reader-fed: 100 KiB body` | `12.06 ms` |
| `http1 parser reader-fed: 1 MiB body` | `103.78 ms` |
| `http1 parser reader-fed: many headers` | `172.94 us` |

## After: Current Public String Entry Point

These are the current means for `Http1.Request.parse`, which now materializes a `StringView`
internally before parsing.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory: small request` | `8.14 us` |
| `http1 parser in-memory: 1 KiB body` | `51.14 us` |
| `http1 parser in-memory: 100 KiB body` | `2.41 ms` |
| `http1 parser in-memory: 1 MiB body` | `24.41 ms` |
| `http1 parser in-memory: many headers` | `253.11 us` |

### Reader-Fed

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed: small request` | `15.50 us` |
| `http1 parser reader-fed: 1 KiB body` | `56.10 us` |
| `http1 parser reader-fed: 100 KiB body` | `14.73 ms` |
| `http1 parser reader-fed: 1 MiB body` | `199.88 ms` |
| `http1 parser reader-fed: many headers` | `253.91 us` |

## After: Direct StringView Entry Point

These are the current means for `Http1.Request.parse_string_view`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory string_view: small request` | `6.86 us` |
| `http1 parser in-memory string_view: 1 KiB body` | `29.22 us` |
| `http1 parser in-memory string_view: 100 KiB body` | `1.22 ms` |
| `http1 parser in-memory string_view: 1 MiB body` | `11.84 ms` |
| `http1 parser in-memory string_view: many headers` | `217.68 us` |

### Reader-Fed

The direct reader-fed `StringView` path currently reads into `Std.IO.IoBuffer` with vectored reads
and then parses `Std.IO.StringView.of_buffer`.

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed string_view: small request` | `12.47 us` |
| `http1 parser reader-fed string_view: 1 KiB body` | `50.05 us` |
| `http1 parser reader-fed string_view: 100 KiB body` | `19.37 ms` |
| `http1 parser reader-fed string_view: 1 MiB body` | `290.60 ms` |
| `http1 parser reader-fed string_view: many headers` | `270.20 us` |

## Current Read

- The parser itself benefits from `StringView` relative to the new public string entry point.
  `parse_string_view` is faster than `parse`, but it is still slower than the original
  string-slicing baseline across the measured in-memory shapes.
- The public `parse : string -> ...` entry point is now much slower than the baseline because it
  first copies the input into off-heap storage before parsing.
- The current reader-fed `StringView` path is also slower than the old reader-fed baseline for large
  requests. The parser is no longer the dominant cost there; `IoBuffer` accumulation is.
- The next meaningful optimization target is not the request parser again. It is the off-heap
  ingestion path: `IoBuffer` growth, fill, and handoff into `StringView`.
