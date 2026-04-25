open Std

(** Error type that wraps driver errors with their conversion functions *)
type error =
  | DriverError : { error: 'err; to_string: 'err -> string; to_json: 'err -> Data.Json.t } -> error

type t

type config =
  | Config : {
    driver: (module Sqlx_driver.Driver.Intf with type config = 'config);
    config: 'config;
  } -> config

val create: config -> (t, error) result

val query: t -> string -> Sqlx_driver.Value.t list -> (Cursor.t, error) result

val execute: t -> string -> Sqlx_driver.Value.t list -> (int, error) result

val ping: t -> bool

val close: t -> unit

val id: t -> string

val created_at: t -> Time.Instant.t

val last_used: t -> Time.Instant.t
