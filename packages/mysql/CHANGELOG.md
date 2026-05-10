# Changelog

All notable changes to `mysql` are documented here.

## 0.0.33 - 2026-05-10

### Added

- Added a MySQL/InnoDB adapter for `sqlx-driver`.
- Added MySQL 4.1+ protocol framing, handshake, authentication, query, prepared statement, and result decoding support.
- Added `mysql_native_password` and `caching_sha2_password` authentication.
- Added optional TLS negotiation for servers that advertise `CLIENT_SSL`.
- Added connection-string parsing for URI and legacy forms.
- Added live MySQL coverage through `testcontainers` using a disposable MySQL container.
- Added property-style live coverage for scalar values, prepared parameters, NULLs, temporal values, blobs, InnoDB CRUD, and rollback behavior.
- Added fuzz coverage for protocol packets, handshake/result parsers, row decoders, statement parameter encoding, and connection-string parsing.
- Added serde serializers for MySQL protocol and driver errors.

### Changed

- Preserved configured hostnames until connect time so TLS SNI uses the original hostname.
- Disabled multi-result client capability advertising until the driver exposes multi-result consumption.
- Added timeout-aware TCP connect, read, and write paths.
- Replaced the remaining `std.data.json` error adapter surface with serde serializers.

### Fixed

- Rejected invalid URI ports instead of silently falling back to the default port.
- Rejected unsupported TCP keepalive configuration explicitly.
- Bounded accumulated packet payloads, result columns, and buffered result rows.
- Rejected invalid length-encoded string sizes before converting them to platform integers.
- Decoded signed binary integer values correctly.
- Returned unsigned 64-bit binary integer values that exceed signed `int64` as numeric strings.
- Hardened short error packet parsing and binary-row null bitmap checks found by fuzzing.
