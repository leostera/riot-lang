# sqlx-driver

Driver interface for `sqlx`.

`sqlx-driver` is the package you implement when you want to add a new database
backend to Riot's SQL layer. It defines the row, value, error, and driver
contracts that concrete adapters such as `sqlite` and `postgres` plug into.

## Should you depend on this directly?

Only if you are writing a database adapter or something extremely close to one.

Most application code should depend on:

- `sqlx` for the high-level query and pooling API;
- `sqlite` or `postgres` for a concrete backend.

## Install

```sh
riot add sqlx-driver
```

## What it contains

- the driver interface a backend must implement;
- value and row abstractions shared across backends;
- a common error vocabulary for SQL-facing code.
- a migration preparation hook so adapters can split or normalize raw migration
  bodies before SQLx executes them.

## Good references

- `packages/sqlite` shows the smallest concrete driver.
- `packages/postgres` shows a more featureful networked backend.
