# sqlite

SQLite driver for `sqlx`.

`sqlite` gives Riot a concrete `sqlx-driver` backend for file-backed and
in-memory SQLite databases. It opens SQLite through native stubs, binds
parameters, returns typed `Sqlx_driver.Row.t` values, and supports transactions.

## Install

```sh
riot add sqlite
```

## Good fits for SQLite

- local developer tools;
- test fixtures and ephemeral databases;
- desktop or embedded applications;
- single-node services that do not need a separate database server.

## Minimal example

```ocaml
open Sqlx

let pool_result =
  Sqlx.connect
    ~config:{ Sqlx.Config.default with pool_size = 1 }
    ~driver:(module Sqlite.Driver)
    (Sqlite.Config.in_memory ())
```

You can swap `Sqlite.Config.in_memory ()` for `Sqlite.Config.default (Path.v
"app.db")` when you want a file on disk.

Use a single connection or a pool size of 1 for private `:memory:` databases.
SQLite file-backed databases work with normal pool sizes.

## Test databases

```ocaml
let test_creates_rows _ctx =
  Sqlite.Testing.with_db (Sqlite.Config.default (Path.v "test.db")) (fun db ->
    (* db is a temporary SQLite database connection for this callback *)
    Ok ())
```

`Sqlite.Testing.with_db` creates file-backed databases inside a temporary
directory and cleans them up after the callback. `Config.in_memory ()` stays
purely in memory.

## What to read

- `src/sqlite.mli` documents the configuration surface in detail.
- `packages/sqlx` is the higher-level API you actually query through.
