open Std

(**
   SQLite adapter for the shared SQLx driver interface.

   The driver opens file-backed or in-memory SQLite databases, binds
   `Sqlx_driver.Value.t` parameters to prepared statements, materializes result
   rows as `Sqlx_driver.Row.t`, and exposes transaction operations through the
   common `sqlx-driver` contract.
*)

(** SQLite connection configuration. *)
module Config: sig
  type mode =
    | ReadOnly
    | ReadWrite
    | Create
  type synchronous =
    | Off
    | Normal
    | Full
    | Extra
  type t = {
    path: Path.t;
    (** Database file path. Use `Config.in_memory ()` for a private in-memory database. *)
    mode: mode;
    (** Database access mode. `Create` opens read/write and creates the file if needed. *)
    busy_timeout: Time.Duration.t option;
    (** How long SQLite waits for locked tables before returning `SQLITE_BUSY`. *)
    cache_size: int option;
    (** Optional `PRAGMA cache_size` value, in SQLite pages. *)
    synchronous: synchronous option;
    (** Optional `PRAGMA synchronous` mode. *)
  }

  (** File-backed defaults: create if missing, 5s busy timeout, normal sync. *)
  val default: Path.t -> t

  (** Private in-memory database defaults for tests and short-lived tools. *)
  val in_memory: unit -> t
end

(** SQLite driver implementation for SQLx. *)
module Driver: Sqlx_driver.Driver.Intf with type config = Config.t

(** Public helpers for rendering and serializing SQLite driver errors. *)
module Error: sig
  type t = Driver.error

  val to_string: t -> string

  val serializer: t Serde.Ser.t
end

(** Test helpers for disposable SQLite databases. *)
module Testing: sig
  (**
     [with_db config fn] opens a disposable SQLite database, runs [fn], closes
     the connection, and removes any temporary file storage.

     For `Config.in_memory ()`, the database is private to this callback. For
     file-backed configs, the configured path is treated as a template and the
     actual database file is created inside a temporary directory.
  *)
  val with_db: Config.t -> (Driver.connection -> ('a, string) result) -> ('a, string) result
end
