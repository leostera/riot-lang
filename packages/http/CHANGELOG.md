# Changelog

All notable changes to `http` are documented here.

## 0.0.26 - 2026-04-28

### Changed

- HTTP/1 parsing is stricter and more complete. Request and response parsers now validate CRLF placement, request targets, response versions, malformed headers, incomplete status/request lines, ambiguous body framing, and fixed body framing.
- HTTP/1 chunked transfer support was expanded for requests and responses, including chunked body decoding, chunk delimiter validation, chunk-size overflow handling, trailer parsing, and line/header block byte limits.
- HTTP cookie parsing and rendering now return structured errors for invalid names, values, max-age fields, same-site values, content-length fields, and set-cookie payloads.
- HTTP/1 server-sent event parsing can now assemble complete SSE events.
- HTTP/2 frame parsing, serialization, and connection handling were hardened across stream id validation, settings validation, empty settings acknowledgements, frame payload sizes, metadata checks, and invalid serialized payloads.
- HTTP/2 stream-state validation now rejects peer protocol violations such as data before headers, data after stream end, headers after stream end, idle-stream control frames, new streams after GOAWAY, invalid stream ordering, unsupported push promises, and excessive concurrent streams.
- HTTP/2 flow control now tracks split windows, applies remote initial windows, rejects window-update overflow, and validates self-dependent priorities.
- HPACK handling now validates dynamic table size update order, resets reader state correctly, rejects unsupported Huffman strings, encodes custom literal header names, and guards integer overflow.
- WebSocket parsing and serialization now validate masking roles, invalid frame encodings, close payloads, extended payload lengths, parser payload limits, and remaining frame bytes.
- WebSocket message assembly was added so fragmented frames can be reconstructed into complete messages.

## 0.0.19 - 2026-04-22

### Changed

- Continued IO/runtime hardening and performance work across `std`, `kernel`, `http`, and `serde-json`, including vectored TLS fallback support and lower-overhead buffer/reader paths.
