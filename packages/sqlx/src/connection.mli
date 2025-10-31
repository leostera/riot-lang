open Std


type t

type config =
  | Config : {
      driver : (module Sqlx_driver.Driver.Intf with type config = 'config);
      config : 'config;
    }
      -> config

val create : config -> (t, string) result
val query : t -> string -> Sqlx_driver.Value.t list -> (Cursor.t, string) result
val execute : t -> string -> Sqlx_driver.Value.t list -> (int, string) result
val ping : t -> bool
val close : t -> unit
val id : t -> string
val created_at : t -> Time.Instant.t
val last_used : t -> Time.Instant.t
