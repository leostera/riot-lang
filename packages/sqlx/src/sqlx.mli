open Std

module ProtocolError: sig
  type t

  val serializer: t Serde.Ser.t

  val to_string: t -> string

  val make: 'err -> serializer:'err Serde.Ser.t -> to_string:('err -> string) -> t
end

type operation =
  | Acquire
  | Query
  | Transaction
type error =
  | PoolError of Pool.error
  | InvalidValue of {
      field: string;
      value: string;
      expected_type: string;
      reason: string option;
    }
  | Timeout of {
      operation: operation;
      duration: Time.Duration.t;
    }

module Config: sig
  type isolation_level =
    | ReadUncommitted
    | ReadCommitted
    | RepeatableRead
    | Serializable
  type t = {
    pool_size: int;
    max_idle_time: Time.Duration.t;
    acquire_timeout: Time.Duration.t;
    idle_check_interval: Time.Duration.t;
    max_lifetime: Time.Duration.t option;
    auto_commit: bool;
    isolation_level: isolation_level option;
    query_timeout: Time.Duration.t option;
    log_queries: bool;
    log_slow_queries: Time.Duration.t option;
  }

  val default: t
end

module Connection: module type of Connection

module Cursor: module type of Cursor

module Row: module type of Sqlx_driver.Row

module Value: module type of Sqlx_driver.Value

module Transaction: module type of Transaction

module Driver: module type of Sqlx_driver.Driver

module Pool: module type of Pool

module Migrate: module type of Migrate

val connect:
  ?config:Config.t ->
  driver:(module Sqlx_driver.Driver.Intf with type config = 'config) ->
  'config ->
  (Pool.t, error) result

val query: Pool.t -> string -> Value.t list -> (Cursor.t, error) result

val exec: Pool.t -> string -> Value.t list -> (int, error) result

(**
   Run database migrations before the application starts handling work.

   By default this resolves SQL migrations from `./migrations`. Use
   `Migrate.run` directly when the caller needs the full migration report.
*)
val migrate:
  ?config:Migrate.Config.t ->
  ?source:Migrate.Source.t ->
  Pool.t ->
  unit ->
  (unit, Migrate.error) result

val with_transaction:
  Pool.t ->
  (Connection.t -> ('a, Connection.error) result) ->
  ('a, error) result

val shutdown: Pool.t -> unit

val show_error: error -> string
