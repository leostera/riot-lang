open Std

type isolation_level =
  | ReadUncommitted
  | ReadCommitted
  | RepeatableRead
  | Serializable

module type Intf = sig
  type config
  type connection
  type statement
  type result_set
  type error

  val name: string

  val error_to_string: error -> string

  val error_serializer: error Serde.Ser.t

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

  val set_isolation_level: connection -> isolation_level -> (unit, error) result
end
