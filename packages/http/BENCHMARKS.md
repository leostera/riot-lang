# HTTP Benchmarks

This file tracks the HTTP/1 request-parser migration from heap-string slicing to
`Std.IO.StringView`.

Date:
- 2026-04-19

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

## Current: Public String Entry Point

These are the current means for `Http1.Request.parse`, which now materializes a `StringView`
internally before parsing.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory: small request` | `7.95 us` |
| `http1 parser in-memory: 1 KiB body` | `18.57 us` |
| `http1 parser in-memory: 100 KiB body` | `104.93 us` |
| `http1 parser in-memory: 1 MiB body` | `356.53 us` |
| `http1 parser in-memory: many headers` | `313.85 us` |

### Reader-Fed

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed: small request` | `13.50 us` |
| `http1 parser reader-fed: 1 KiB body` | `30.53 us` |
| `http1 parser reader-fed: 100 KiB body` | `9.73 ms` |
| `http1 parser reader-fed: 1 MiB body` | `135.64 ms` |
| `http1 parser reader-fed: many headers` | `231.92 us` |

## Current: Direct StringView Entry Point

These are the current means for `Http1.Request.parse_string_view`.

### Parser Only

| Benchmark | Mean |
| --- | ---: |
| `http1 parser in-memory string_view: small request` | `9.36 us` |
| `http1 parser in-memory string_view: 1 KiB body` | `10.85 us` |
| `http1 parser in-memory string_view: 100 KiB body` | `17.37 us` |
| `http1 parser in-memory string_view: 1 MiB body` | `250.67 us` |
| `http1 parser in-memory string_view: many headers` | `239.31 us` |

### Reader-Fed

The direct reader-fed `StringView` path currently reads into `Std.IO.IoBuffer` with vectored reads
and then parses `Std.IO.StringView.from_buffer`.

| Benchmark | Mean |
| --- | ---: |
| `http1 parser reader-fed string_view: small request` | `15.96 us` |
| `http1 parser reader-fed string_view: 1 KiB body` | `26.09 us` |
| `http1 parser reader-fed string_view: 100 KiB body` | `6.11 ms` |
| `http1 parser reader-fed string_view: 1 MiB body` | `75.11 ms` |
| `http1 parser reader-fed string_view: many headers` | `241.80 us` |

## Current Read

- Large-body request parsing now benefits clearly from the off-heap path. Both the public parser and
  the direct `StringView` parser beat the old baseline on `100 KiB` and `1 MiB` request shapes.
- The direct `parse_string_view` path is now the best measured path for large request bodies,
  especially on the reader-fed benchmark where `1 MiB` dropped from `103.78 ms` to `75.11 ms`.
- Tiny requests and header-heavy shapes are still mixed: setup overhead dominates there, so the old
  string baseline can remain competitive or faster.
- The next optimization target is still the ingestion path rather than the request parser itself:
  `IoBuffer` growth, fill, and handoff into `StringView` are where additional wins should come from.
