# sqlx

High-level SQL API for Riot.

`sqlx` gives you a database-facing API with connection pooling, transactions,
queries, typed value/row abstractions, and migration management. It does not tie
you to one backend: you provide a concrete driver such as `sqlite`, `postgres`,
or `mysql`.

## Install

```sh
riot add sqlx
```

## What you get

- connection pool management;
- query and exec helpers over a shared value model;
- transaction support;
- SQL migration resolution and execution;
- a backend-neutral surface that works with multiple drivers.

## Minimal query shape

```ocaml
open Sqlx

let start = fun database_url ->
  match Postgres.Config.from_string database_url with
  | Error message -> Error message
  | Ok config -> (
      match Sqlx.connect ~driver:(module Postgres.Driver) config with
      | Error error -> Error (Sqlx.show_error error)
      | Ok pool -> Ok pool
    )
```

From there you can call `Sqlx.query`, `Sqlx.exec`, and
`Sqlx.with_transaction`.

## Migrations

Application schema should live in SQL migration files, not inline strings inside
runtime modules. By default, `Sqlx.migrate pool ()` resolves files from
`./migrations`, relative to the process working directory:

```ocaml
let start = fun database_url ->
  match Postgres.Config.from_string database_url with
  | Error message -> Error message
  | Ok config -> (
      match Sqlx.connect ~driver:(module Postgres.Driver) config with
      | Error error -> Error (Sqlx.show_error error)
      | Ok pool -> (
          let migration_config = Sqlx.Migrate.Config.for_postgres () in
          match Sqlx.migrate ~config:migration_config pool () with
          | Ok () -> Ok pool
          | Error error ->
              Sqlx.shutdown pool;
              Error (Sqlx.Migrate.error_to_string error)
        )
    )
```

Migration filenames follow the same shape as `sqlx-rs`:

- `1_create_users.sql` for a simple forward-only migration;
- `2_add_orders.up.sql` for the forward side of a reversible migration;
- `2_add_orders.down.sql` for the rollback side of a reversible migration.

Descriptions are derived from the filename after the version prefix, with
underscores rendered as spaces. Migrations are sorted by numeric version before
execution.

Use `-- no-transaction` as the first line when a migration must run outside a
transaction. Prefer transactional migrations whenever the database supports
them. MySQL configs default to non-transactional migration bodies because MySQL
DDL commonly commits implicitly.

Use `Sqlx.Migrate.run` instead of `Sqlx.migrate` when you need the full report
of applied migrations and already-applied versions:

```ocaml
let source = Sqlx.Migrate.Source.from_directory (Path.v "migrations")
let config = Sqlx.Migrate.Config.for_postgres ()
let result = Sqlx.Migrate.run ~config pool source
```

For MySQL/InnoDB, use the MySQL config helper:

```ocaml
let source = Sqlx.Migrate.Source.from_directory (Path.v "migrations")
let config = Sqlx.Migrate.Config.for_mysql ()
let result = Sqlx.Migrate.run ~config pool source
```

If your application starts from a different working directory, pass an explicit
source:

```ocaml
let source = Sqlx.Migrate.Source.from_directory (Path.v "/opt/app/migrations")
let result = Sqlx.migrate ~source pool ()
```

The migrator stores applied versions, checksums, and execution durations in
`_sqlx_migrations` by default. On startup it rejects dirty databases, missing
applied migrations, and modified applied migrations unless the migration config
explicitly opts out of the missing-file check.

Backend-specific migration config controls placeholders, migration-table DDL,
locking, and transaction defaults. PostgreSQL uses advisory locks and `$1`
placeholders. MySQL uses `GET_LOCK`/`RELEASE_LOCK`, `?` placeholders, and an
InnoDB migration table.

## Which driver should you use?

- `sqlite` is great for local tools, tests, and embedded data.
- `postgres` is the choice when you need a long-running server database.
- `mysql` is the MySQL/InnoDB driver and uses `?` placeholders.

## Where to start

- `src/sqlx.mli` is the main public API.
- `src/migrate.mli` documents migration resolution and execution.
- `tests/sqlx_tests.ml`, `tests/pool_tests.ml`, and `tests/migrate_tests.ml`
  show the expected behavior.
