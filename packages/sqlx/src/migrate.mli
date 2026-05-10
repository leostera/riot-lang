open Std

(**
   SQL migration management.

   This module follows the same operational model as sqlx-rs migrations:
   resolve ordered SQL files, validate applied migration checksums, reject
   dirty databases, and apply only pending migrations. Backend-specific
   behavior, such as PostgreSQL advisory locks and MySQL named locks, is
   explicit in `locking`.

   Directory sources resolve SQL files named with a numeric version prefix:
   - `1_create_users.sql` for a simple forward-only migration.
   - `2_add_orders.up.sql` for the forward side of a reversible migration.
   - `2_add_orders.down.sql` for the rollback side of a reversible migration.

   The version is parsed from the prefix before the first underscore. The
   description is derived from the remaining filename with underscores rendered
   as spaces. Files are sorted by numeric version before execution.

   Put `-- no-transaction` at the start of a migration file when the database
   requires the migration body to run outside a transaction. MySQL configs use
   non-transactional migration bodies by default because MySQL DDL commonly
   commits implicitly.
*)
module Version: sig
  type t
  type error =
    | NotPositive of int64
    | InvalidInteger of string
    | ExpectedIntegerValue

  val from_int: int -> (t, error) result

  val from_int64: int64 -> (t, error) result

  val from_string: string -> (t, error) result

  val from_int64_unchecked: int64 -> t

  val to_int64: t -> int64

  val to_string: t -> string

  val equal: t -> t -> bool

  val compare: t -> t -> Order.t

  val error_to_string: error -> string
end

module TableName: sig
  type t
  type error =
    | Empty
    | InvalidIdentifier of string

  val default: t

  val from_string: string -> (t, error) result

  val from_string_unchecked: string -> t

  val to_string: t -> string

  val error_to_string: error -> string
end

type migration_type =
  | Simple
  | ReversibleUp
  | ReversibleDown

val migration_type_to_string: migration_type -> string

module Migration: sig
  type t = private {
    version: Version.t;
    description: string;
    migration_type: migration_type;
    sql: string;
    checksum: string;
    no_tx: bool;
  }

  val make:
    ?no_tx:bool ->
    ?checksum:string ->
    version:Version.t ->
    description:string ->
    migration_type:migration_type ->
    sql:string ->
    unit ->
    t
end

module AppliedMigration: sig
  type t = {
    version: Version.t;
    checksum: string;
  }
end

type locking =
  | NoLock
  | PostgresAdvisory of {
      key: int64;
    }
  (** PostgreSQL advisory lock. *)
  | MysqlNamed of {
      name: string;
      timeout: Time.Duration.t;
    }
type dialect =
  | Postgres
  | Mysql
type transaction_mode =
  | Transactional
  | NonTransactional

module Config: sig
  type t = {
    table_name: TableName.t;
    ignore_missing: bool;
    dialect: dialect;
    locking: locking;
    transaction_mode: transaction_mode;
    create_schemas: string Collections.Vector.t;
  }

  val default: t

  val for_postgres: ?lock_key:int64 -> unit -> t

  (**
     Build a MySQL/InnoDB migration config.

     This selects [?] placeholders, creates the migration table with
     [ENGINE=InnoDB], uses [GET_LOCK]/[RELEASE_LOCK], and runs migration bodies
     outside an explicit transaction by default.
  *)
  val for_mysql: ?lock_name:string -> ?lock_timeout:Time.Duration.t -> unit -> t
end

type applied = {
  migration: Migration.t;
  elapsed: Time.Duration.t;
}
type run_report = {
  applied: applied Collections.Vector.t;
  already_applied: AppliedMigration.t Collections.Vector.t;
}
(** Errors raised while resolving migration sources or reading migration metadata. *)
type source_error =
  | ReadMigrationFileFailed of {
      path: Path.t;
      reason: string;
    }
  | ReadMigrationDirectoryFailed of {
      path: Path.t;
      reason: string;
    }
  | InspectMigrationPathFailed of {
      path: Path.t;
      reason: string;
    }
  | MissingQueryField of string
  | QueryFieldTypeMismatch of { field: string; expected: string }
type error =
  | SourceError of source_error
  | InvalidVersion of Version.error
  | InvalidTableName of TableName.error
  | InvalidSchemaName of TableName.error
  | PoolError of Pool.error
  | ConnectionError of Connection.error
  | MigrationExecutionError of {
      version: Version.t;
      error: Connection.error;
    }
  | Dirty of Version.t
  | LockUnavailable of string
  | VersionMissing of Version.t
  | VersionMismatch of Version.t
  | VersionNotPresent of Version.t

module Source: sig
  type resolve_config = {
    ignored_checksum_chars: char Collections.Vector.t;
  }

  val default_resolve_config: resolve_config

  type t

  val from_directory: Path.t -> t

  val from_migrations: Migration.t Collections.Vector.t -> t

  val resolve: ?config:resolve_config -> t -> (Migration.t Collections.Vector.t, error) result
end

val error_to_string: error -> string

val list_applied:
  ?config:Config.t ->
  Pool.t ->
  (AppliedMigration.t Collections.Vector.t, error) result

val run: ?config:Config.t -> Pool.t -> Source.t -> (run_report, error) result

val run_to: ?config:Config.t -> Pool.t -> Source.t -> target:Version.t -> (run_report, error) result

val undo: ?config:Config.t -> Pool.t -> Source.t -> target:Version.t -> (run_report, error) result
