# postgres

PostgreSQL driver for `sqlx`.

`postgres` is Riot's networked SQL driver for PostgreSQL. It implements the
database wire protocol and plugs into `sqlx` so application code can work
through one consistent query, pool, and transaction surface.

## Install

```sh
riot add postgres
```

## Use it when

- you want a real server database rather than a local file;
- you need concurrent readers/writers and normal production database features;
- you already plan to use PostgreSQL-specific capabilities such as JSON, arrays,
  or LISTEN/NOTIFY.

## Minimal shape

```ocaml
open Sqlx

let config = {
  (Postgres.Config.default ()) with
  host = "127.0.0.1";
  port = 5432;
  database = "app";
  user = "app";
  password = Env.get "DATABASE_PASSWORD" |> Option.unwrap_or ~default:"";
  ssl_mode = Postgres.Config.Disable;
} in
let pool = Sqlx.connect ~driver:(module Postgres.Driver) config
```

In practice you will usually override at least the host, database, user, and
password fields on the config.

## Connection strings

`Postgres.Config.from_string` accepts two forms:

```text
postgresql://user:password@localhost:5432/database
postgres://user:password@localhost:5432/database
host:port:database:user:password
```

The parser returns `Error string` for malformed input instead of raising. URI
query parameters are not interpreted yet; set fields such as `ssl_mode` on the
returned config explicitly.

## Authentication

The driver supports PostgreSQL startup plus these authentication requests:

- cleartext password;
- MD5 password;
- `SCRAM-SHA-256`.

Unsupported authentication mechanisms return a structured driver error. SCRAM
server signatures are verified before the connection is considered ready.

## Parameters and NULL

Parameterized queries use PostgreSQL's extended protocol when parameters are
present. `Sqlx_driver.Value.Null` is encoded as a PostgreSQL NULL parameter
with a `-1` value length. Empty strings are encoded as present values with a
zero byte length, so `NULL` and `""` remain distinct on the wire.

## Transactions

`Sqlx.Transaction` calls are sent to PostgreSQL:

- `begin_transaction` sends `BEGIN`;
- `commit` sends `COMMIT`;
- `rollback` sends `ROLLBACK`;
- `set_isolation_level` sends the matching `SET TRANSACTION ...` command while
  a transaction is active, or `SET SESSION CHARACTERISTICS ...` otherwise.

The driver tracks the backend `ReadyForQuery` transaction status after query
execution and rejects nested `begin_transaction` calls on the same connection.

## Current limitations

- TLS negotiation is not implemented yet. `ssl_mode = Require` fails clearly
  instead of silently opening a plaintext connection. `Prefer` currently uses
  the plaintext path.
- `connect_timeout` and `keepalives_idle` are reserved in the config surface but
  are not wired into the TCP connection path yet.
- Tests in this package focus on protocol encoding and decoding without
  requiring a live PostgreSQL server. End-to-end database coverage should be
  added with an explicit integration test harness.

## Where to start

- `src/postgres.mli` documents the config surface and supported protocol
  features.
- use it through `packages/sqlx` rather than calling into the driver directly.
