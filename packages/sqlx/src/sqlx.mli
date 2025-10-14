open Std

type error =
  | Connection_failed of string
  | Query_failed of string
  | Pool_exhausted
  | Invalid_value of string
  | Driver_error of string

module Config : sig
  type t = {
    pool_size : int;
    max_idle_time : Time.Duration.t;
    acquire_timeout : Time.Duration.t;
    idle_check_interval : Time.Duration.t;
    max_lifetime : Time.Duration.t option;
    auto_commit : bool;
    isolation_level :
      [ `Read_uncommitted | `Read_committed | `Repeatable_read | `Serializable ]
      option;
    query_timeout : Time.Duration.t option;
    log_queries : bool;
    log_slow_queries : Time.Duration.t option;
  }

  val default : t
end

module Connection : module type of Connection
module Cursor : module type of Cursor
module Row : module type of Sqlx_driver.Row
module Value : module type of Sqlx_driver.Value
module Transaction : module type of Transaction
module Driver : module type of Sqlx_driver.Driver
module Pool : module type of Pool

val connect :
  ?config:Config.t ->
  driver:(module Sqlx_driver.Driver.Intf with type config = 'config) ->
  'config ->
  (Pool.t, error) result

val query : Pool.t -> string -> Value.t list -> (Cursor.t, error) result
val exec : Pool.t -> string -> Value.t list -> (int, error) result

val with_transaction :
  Pool.t -> (Connection.t -> ('a, string) result) -> ('a, string) result

val shutdown : Pool.t -> unit
val show_error : error -> string
