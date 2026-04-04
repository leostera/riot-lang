# sqlx

High-level SQL API for Riot.

`sqlx` gives you a database-facing API with connection pooling, transactions,
queries, and typed value/row abstractions. It does not tie you to one backend:
you provide a concrete driver such as `sqlite` or `postgres`.

## Install

```sh
riot add sqlx
```

## What you get

- connection pool management;
- query and exec helpers over a shared value model;
- transaction support;
- a backend-neutral surface that works with multiple drivers.

## Minimal shape

```ocaml
open Sqlx

let pool =
  Sqlx.connect
    ~driver:(module Sqlite.Driver)
    (Sqlite.Config.in_memory ())
```

From there you can call `Sqlx.query`, `Sqlx.exec`, and
`Sqlx.with_transaction`.

## Which driver should you use?

- `sqlite` is great for local tools, tests, and embedded data.
- `postgres` is the choice when you need a long-running server database.

## Where to start

- `src/sqlx.mli` is the main public API.
- `tests/test_sqlx.ml` and `tests/test_pool.ml` show the expected behavior.
