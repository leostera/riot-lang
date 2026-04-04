# http

HTTP protocol parsing and wire-level types for Riot.

`http` is the package to reach for when you need HTTP message parsing,
serialization, status/header types, or lower-level protocol building blocks.
It sits below `blink` and `suri`, which provide the higher-level client and
server stories.

## Install

```sh
riot add http
```

## What this package is good at

- HTTP/1 request and response parsing;
- HPACK support and frame serialization needed for modern HTTP transports;
- wire-level types and helpers you can build custom clients or servers on top
  of;
- a protocol-focused surface that does not force a server or client framework.

## When not to use it directly

If you want to make requests, use `blink`.

If you want to run a web server, use `suri`.

Use `http` directly when you are implementing protocol machinery, transport
layers, proxies, or custom tooling that needs to understand HTTP on the wire.

## Where to start

- `src/http.mli` is the public entrypoint.
- `tests/http1_parser_tests.ml` shows the request/response parsing surface.
- `tests/hpack_tests.ml` and `tests/parser_tests.ml` cover the lower-level
  protocol pieces.
