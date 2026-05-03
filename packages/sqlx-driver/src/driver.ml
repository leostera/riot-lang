open Std

module type Intf = sig
  type config
  type connection
  type statement
  type result_set
  type error

  val name: string

  (* Error conversion - optional but recommended *)

  (* Error conversion - optional but recommended *)
  val error_to_string: error -> string

  val error_to_json: error -> Data.Json.t

  val connect: config -> (connection, error) result

  val close: connection -> unit

  val ping: connection -> bool

  val prepare: connection -> string -> (statement, error) result

  val execute: statement -> Value.t list -> (result_set, error) result

  val fetch_row: result_set -> Row.t option

  val rows_affected: result_set -> int

  val begin_transaction: connection -> (unit, error) result

  val commit: connection -> (unit, error) result

  val rollback: connection -> (unit, error) result

  val set_isolation_level:
    connection ->
    [ | `Read_uncommitted | `Read_committed | `Repeatable_read | `Serializable] ->
    (unit, error) result
end
