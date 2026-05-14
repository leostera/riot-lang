open Std

type error =
  | Exhausted of {
      waiting: int;
      max_connections: int;
      timeout: Time.Duration.t;
    }
  | ConnectionError of Connection.error
  | Timeout of Time.Duration.t
type config =
  | Config: {
      driver: (module Sqlx_driver.Driver.Intf with type config = 'config);
      driver_config: 'config;
      min_connections: int;
      max_connections: int;
      acquire_timeout: Time.Duration.t;
      idle_timeout: Time.Duration.t;
      max_lifetime: Time.Duration.t option;
    } -> config
type t
type stat =
  | TotalConnections of int
  | AvailableConnections of int
  | InUseConnections of int
  | WaitingRequests of int

val create: config -> (t, Connection.error) result

val acquire: t -> (Connection.t, error) result

val release: t -> Connection.t -> unit

val with_connection: t -> (Connection.t -> ('a, Connection.error) result) -> ('a, error) result

val shutdown: t -> unit

val stats: t -> stat list
