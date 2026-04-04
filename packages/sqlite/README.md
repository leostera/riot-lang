# sqlite

SQLite driver for `sqlx`.

`sqlite` gives Riot a concrete `sqlx` backend for file-backed and in-memory
SQLite databases. It is the easiest database story in the stack when you want
something small, local, and dependency-free.

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

let pool =
  Sqlx.connect
    ~driver:(module Sqlite.Driver)
    (Sqlite.Config.in_memory ())
```

You can swap `Sqlite.Config.in_memory ()` for `Sqlite.Config.default (Path.v
"app.db")` when you want a file on disk.

## What to read

- `src/sqlite.mli` documents the configuration surface in detail.
- `packages/sqlx` is the higher-level API you actually query through.
