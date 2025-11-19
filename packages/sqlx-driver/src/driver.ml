open Std

module type Intf = sig
  type config
  type connection
  type statement
  type result_set

  val name : string
  val connect : config -> (connection, string) result
  val close : connection -> unit
  val ping : connection -> bool
  val prepare : connection -> string -> (statement, string) result
  val execute : statement -> Value.t list -> (result_set, string) result
  val fetch_row : result_set -> Row.t option
  val rows_affected : result_set -> int
  val begin_transaction : connection -> (unit, string) result
  val commit : connection -> (unit, string) result
  val rollback : connection -> (unit, string) result

  val set_isolation_level :
    connection ->
    [ `Read_uncommitted | `Read_committed | `Repeatable_read | `Serializable ] ->
    (unit, string) result
end
