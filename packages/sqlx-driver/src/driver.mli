open Std

(**
   # Database Driver Interface

   SQLx drivers implement this interface to expose database-specific
   connection, query, result, and transaction behavior through a uniform API.

   ## Implementing a Driver

   1. Define the configuration type.
   2. Define internal types for connections, statements, and result sets.
   3. Implement every operation in `Intf`.
   4. Release resources in `close`.
*)

(** Transaction isolation level. *)
type isolation_level =
  | ReadUncommitted
  | ReadCommitted
  | RepeatableRead
  | Serializable

(** The interface that all database drivers must implement. *)
module type Intf = sig
  (** Driver-specific configuration. *)
  type config

  (** Active driver-specific database connection. *)
  type connection

  (** Driver-specific prepared statement. *)
  type statement

  (** Driver-specific result set. *)
  type result_set

  (** Driver-specific structured error. *)
  type error

  (** Database driver name, such as `PostgreSQL` or `SQLite`. *)
  val name: string

  (** Render a driver error as a human-readable string. *)
  val error_to_string: error -> string

  (** Render a driver error as JSON. *)
  val error_to_json: error -> Data.Json.t

  (**
     Establish a new database connection.

     The driver should perform network setup, authentication, and any initial
     session configuration required by the database.
  *)
  val connect: config -> (connection, error) result

  (** Close the database connection and release its resources. *)
  val close: connection -> unit

  (** Return whether the connection is still alive. *)
  val ping: connection -> bool

  (**
     Prepare a SQL statement for execution.

     Prepared statements allow repeated execution and let drivers bind
     parameters without string interpolation.
  *)
  val prepare: connection -> string -> (statement, error) result

  (**
     Execute a prepared statement with the given parameters.

     Placeholder syntax is driver-specific. PostgreSQL uses `$1`, `$2`, and so
     on, while SQLite-style drivers usually use `?` placeholders.
  *)
  val execute: statement -> Value.t list -> (result_set, error) result

  (** Fetch the next row from a result set, or `None` when exhausted. *)
  val fetch_row: result_set -> Row.t option

  (** Return the number of rows affected by the query. *)
  val rows_affected: result_set -> int

  (** Begin a database transaction on the connection. *)
  val begin_transaction: connection -> (unit, error) result

  (** Commit the current transaction. *)
  val commit: connection -> (unit, error) result

  (** Roll back the current transaction. *)
  val rollback: connection -> (unit, error) result

  (**
     Set the transaction isolation level.

     Not all databases support every isolation level.
  *)
  val set_isolation_level:
    connection -> isolation_level -> (unit, error) result
end
