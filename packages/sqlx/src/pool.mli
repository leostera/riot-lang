open Std


type config =
  | Config : {
      driver : (module Sqlx_driver.Driver.Intf with type config = 'config);
      driver_config : 'config;
      min_connections : int;
      max_connections : int;
      acquire_timeout : Time.Duration.t;
      idle_timeout : Time.Duration.t;
      max_lifetime : Time.Duration.t option;
    }
      -> config

type t

val create : config -> (t, string) result
val acquire : t -> (Connection.t, string) result
val release : t -> Connection.t -> unit

val with_connection :
  t -> (Connection.t -> ('a, string) result) -> ('a, string) result

val shutdown : t -> unit

val stats :
  t ->
  [ `Total of int | `Available of int | `InUse of int | `Waiting of int ] list
