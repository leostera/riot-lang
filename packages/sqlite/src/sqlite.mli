open Std

(* SQLite Database Driver

   This module provides a SQLite driver implementation for SQLx.
   SQLite is a lightweight, file-based SQL database that's perfect
   for development, testing, and embedded applications.

   ## Features

   - File-based storage (single file contains entire database)
   - In-memory databases for testing
   - Zero-configuration
   - ACID transactions
   - Rich SQL support including CTEs, window functions, etc.

   ## Example Usage

   ```ocaml
   open Sqlx

   (* Connect to a file database *)
   let file_config = Sqlite.Config.default (Path.v "myapp.db") in
   let pool = Sqlx.connect ~driver:(module Sqlite.Driver) file_config in

   (* Use in-memory database for tests *)
   let memory_config = Sqlite.Config.in_memory () in
   let test_pool = Sqlx.connect ~driver:(module Sqlite.Driver) memory_config in

   (* Execute queries *)
   Sqlx.exec pool "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)" []
   ```

   ## Limitations

   - No native network access (file-based only)
   - Limited concurrent write access (uses file locking)
   - No user management or access control
   - Doesn't support full SQL standard isolation levels
*)

(* Configuration for SQLite connections *)
module Config: sig
  (* SQLite connection configuration *)
  type t = {
    path: Path.t;
    (* Path to the database file. Use ":memory:" for in-memory databases. *)
    mode: [`ReadOnly | `ReadWrite | `Create];
    (* Database access mode:
       - `ReadOnly`: Open existing database for reading only
       - `ReadWrite`: Open existing database for reading and writing
       - `Create`: Create database if it doesn't exist (implies ReadWrite)
    *)
    busy_timeout: Time.Duration.t option;
    (* How long to wait when the database is locked before returning an error.
       `None` means return immediately with SQLITE_BUSY.
    *)
    cache_size: int option;
    (* Size of the page cache in pages (default is -2000, meaning 2MB).
       Negative values specify cache size in KB.
    *)
    synchronous: ([`Off | `Normal | `Full | `Extra]) option;
    (* Synchronous mode controls how SQLite waits for data to reach persistent storage:
       - `Off`: No syncs (fast but unsafe)
       - `Normal`: Sync at critical moments (balanced)
       - `Full`: Sync after every critical operation (safe but slower)
       - `Extra`: Like Full but with extra syncs for durability
    *)
  }

  (* `default path` creates a configuration for a file-based database at `path`.

     Default settings:
     - mode = `Create`
     - busy_timeout = 5 seconds
     - cache_size = default (-2000)
     - synchronous = `Normal`
  *)
  val default: Path.t -> t

  (* `in_memory ()` creates a configuration for an in-memory database.

     In-memory databases are:
     - Very fast (no disk I/O)
     - Temporary (destroyed when connection closes)
     - Perfect for testing

     Settings optimized for performance:
     - synchronous = `Off`
     - No busy timeout (single connection)
  *)

  (* `in_memory ()` creates a configuration for an in-memory database.

     In-memory databases are:
     - Very fast (no disk I/O)
     - Temporary (destroyed when connection closes)
     - Perfect for testing

     Settings optimized for performance:
     - synchronous = `Off`
     - No busy timeout (single connection)
  *)
  val in_memory: unit -> t
end

(* SQLite driver implementation for SQLx *)
module Driver: Sqlx_driver.Driver.Intf with type config = Config.t
