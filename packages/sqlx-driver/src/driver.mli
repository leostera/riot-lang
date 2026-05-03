open Std

(* Database driver interface.

   This module defines the interface that all database drivers must implement
   to work with the SQLx library. Each driver handles the specifics of
   communicating with its database system while providing a uniform interface.

   ## Implementing a Driver

   To implement a new database driver:

   1. Define your configuration type
   2. Define internal types for connection, statement, and result set
   3. Implement all the required functions
   4. Ensure proper resource cleanup in `close`

   ## Example Implementation

   ```ocaml
   module MyDriver = struct
     type config = { host : string; port : int }
     type connection = { socket : Fd.t; ... }
     type statement = { sql : string; ... }
     type result_set = { rows : Row.t list; ... }

     let name = "MyDatabase"

     let connect config =
       (* Establish connection *)
       Ok { socket = ...; ... }

     let execute stmt params =
       (* Send query and get results *)
       Ok { rows = [...]; ... }

     (* ... implement other functions ... *)
   end
   ```
*)

(* The interface that all database drivers must implement *)
module type Intf = sig
  (* ## Types *)

  (* Driver-specific configuration type.
     This should contain all parameters needed to establish a connection.
  *)
  type config
  (* Driver-specific connection handle.
     This represents an active connection to the database.
  *)
  type connection
  (* Driver-specific prepared statement.
     This represents a parsed and prepared SQL statement.
  *)
  type statement
  (* Driver-specific result set.
     This contains the results of executing a query.
  *)
  type result_set
  (* Driver-specific error type.
     This allows drivers to preserve structured error information.
  *)
  type error

  (* ## Driver Information *)

  (* The name of the database driver (e.g., "PostgreSQL", "SQLite") *)
  val name: string

  (* ## Error Conversion *)

  (* ## Error Conversion *)

  (* Convert driver error to human-readable string *)
  val error_to_string: error -> string

  (* Convert driver error to JSON for serialization *)

  (* Convert driver error to JSON for serialization *)
  val error_to_json: error -> Data.Json.t

  (* ## Connection Management *)

  (* ## Connection Management *)

  (* `connect config` establishes a new connection to the database.
     Returns `Ok connection` on success or `Error error` on failure.

     The driver should:
     - Establish network connection (if applicable)
     - Perform authentication
     - Set up any initial session parameters
  *)
  val connect: config -> (connection, error) result

  (* `close conn` closes the database connection and releases all resources.
     This should be safe to call multiple times.
  *)

  (* `close conn` closes the database connection and releases all resources.
     This should be safe to call multiple times.
  *)
  val close: connection -> unit

  (* `ping conn` checks if the connection is still alive.
     Returns `true` if the connection is active, `false` otherwise.
  *)

  (* `ping conn` checks if the connection is still alive.
     Returns `true` if the connection is active, `false` otherwise.
  *)
  val ping: connection -> bool

  (* ## Query Execution *)

  (* ## Query Execution *)

  (* `prepare conn sql` prepares a SQL statement for execution.
     Returns `Ok statement` on success or `Error error` on failure.

     Preparing statements allows for:
     - Better performance when executing the same query multiple times
     - Protection against SQL injection when using parameters
  *)
  val prepare: connection -> string -> (statement, error) result

  (* `execute stmt params` executes a prepared statement with the given parameters.
     Returns `Ok result_set` on success or `Error error` on failure.

     Parameters are substituted for placeholders in the prepared statement.
     Different databases use different placeholder syntax:
     - PostgreSQL: $1, $2, $3, ...
     - SQLite/MySQL: ?, ?, ?, ...
  *)

  (* `execute stmt params` executes a prepared statement with the given parameters.
     Returns `Ok result_set` on success or `Error error` on failure.

     Parameters are substituted for placeholders in the prepared statement.
     Different databases use different placeholder syntax:
     - PostgreSQL: $1, $2, $3, ...
     - SQLite/MySQL: ?, ?, ?, ...
  *)
  val execute: statement -> Value.t list -> (result_set, error) result

  (* ## Result Processing *)

  (* ## Result Processing *)

  (* `fetch_row result_set` fetches the next row from the result set.
     Returns `Some row` if a row is available, `None` when no more rows.

     This function should be called repeatedly to iterate through all results.
  *)
  val fetch_row: result_set -> Row.t option

  (* `rows_affected result_set` returns the number of rows affected by the query.
     For INSERT, UPDATE, DELETE queries, this is the number of rows modified.
     For SELECT queries, this may return 0 or the total row count depending on the driver.
  *)

  (* `rows_affected result_set` returns the number of rows affected by the query.
     For INSERT, UPDATE, DELETE queries, this is the number of rows modified.
     For SELECT queries, this may return 0 or the total row count depending on the driver.
  *)
  val rows_affected: result_set -> int

  (* ## Transaction Management *)

  (* ## Transaction Management *)

  (* `begin_transaction conn` starts a new database transaction.
     Returns `Ok ()` on success or `Error error` on failure.

     After calling this, all subsequent operations on the connection
     are part of the transaction until `commit` or `rollback` is called.
  *)
  val begin_transaction: connection -> (unit, error) result

  (* `commit conn` commits the current transaction.
     Returns `Ok ()` on success or `Error error` on failure.

     This makes all changes in the transaction permanent.
  *)

  (* `commit conn` commits the current transaction.
     Returns `Ok ()` on success or `Error error` on failure.

     This makes all changes in the transaction permanent.
  *)
  val commit: connection -> (unit, error) result

  (* `rollback conn` rolls back the current transaction.
     Returns `Ok ()` on success or `Error error` on failure.

     This discards all changes made in the transaction.
  *)

  (* `rollback conn` rolls back the current transaction.
     Returns `Ok ()` on success or `Error error` on failure.

     This discards all changes made in the transaction.
  *)
  val rollback: connection -> (unit, error) result

  (* `set_isolation_level conn level` sets the transaction isolation level.
     Returns `Ok ()` on success or `Error error` on failure.

     Isolation levels (from least to most isolated):
     - `Read_uncommitted`: Can read uncommitted changes from other transactions
     - `Read_committed`: Can only read committed changes
     - `Repeatable_read`: Repeated reads return the same data
     - `Serializable`: Transactions execute as if they were serial

     Not all databases support all isolation levels.
  *)

  (* `set_isolation_level conn level` sets the transaction isolation level.
     Returns `Ok ()` on success or `Error error` on failure.

     Isolation levels (from least to most isolated):
     - `Read_uncommitted`: Can read uncommitted changes from other transactions
     - `Read_committed`: Can only read committed changes
     - `Repeatable_read`: Repeated reads return the same data
     - `Serializable`: Transactions execute as if they were serial

     Not all databases support all isolation levels.
  *)
  val set_isolation_level:
    connection ->
    [`Read_uncommitted | `Read_committed | `Repeatable_read | `Serializable] ->
    (unit, error) result
end
