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

let config = Postgres.Config.default () in
let pool = Sqlx.connect ~driver:(module Postgres.Driver) config
```

In practice you will usually override at least the host, database, user, and
password fields on the config.

## Where to start

- `src/postgres.mli` documents the config surface and supported protocol
  features.
- use it through `packages/sqlx` rather than calling into the driver directly.
