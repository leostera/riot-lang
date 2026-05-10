open Std

(** Error type that wraps driver errors with their conversion functions *)
type runtime_error =
  | RandomFailure of {
      label: string;
      reason: string;
    }
  | RaisedException of string
  | InvalidConfiguration of string

type error =
  | DriverError: {
      error: 'err;
      to_string: 'err -> string;
      to_json: 'err -> Data.Json.t;
    } -> error
  | RuntimeError of runtime_error

val error_to_string : error -> string

val error_to_json : error -> Data.Json.t

type t
type config =
  | Config: {
      driver: (module Sqlx_driver.Driver.Intf with type config = 'config);
      config: 'config;
    } -> config

val create : config -> (t, error) result

val query : t -> string -> Sqlx_driver.Value.t list -> (Cursor.t, error) result

val execute : t -> string -> Sqlx_driver.Value.t list -> (int, error) result

val ping : t -> bool

val close : t -> unit

val begin_transaction : t -> (unit, error) result

val commit : t -> (unit, error) result

val rollback : t -> (unit, error) result

val set_isolation_level :
  t ->
  Sqlx_driver.Driver.isolation_level ->
  (unit, error) result

val id : t -> string

val created_at : t -> Time.Instant.t

val last_used : t -> Time.Instant.t

val pool_lease : t -> int

val set_pool_lease : t -> int -> unit
