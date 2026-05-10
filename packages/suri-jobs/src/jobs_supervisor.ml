open Std

type t = {
  runtime: Std.Supervisor.t;
  db: Sqlx.Pool.t;
}

type start_error =
  | ConfigError of Std.Config.error
  | StartError of Error.t

let start_error_to_string = fun error ->
  match error with
  | ConfigError error -> Std.Config.error_to_string error
  | StartError error -> Error.to_string error

let start_link_with_config = fun ~config queues ->
  match Runner.start_db config with
  | Error error -> Error (StartError error)
  | Ok db ->
      let runtime = Runner.start_link_with_db ~db queues in
      Ok { runtime; db }

let start_link = fun queues ->
  match Jobs_config.load () with
  | Error error -> Error (ConfigError error)
  | Ok config -> start_link_with_config ~config queues

let runtime = fun t -> t.runtime

let database = fun t -> t.db

let stop = fun t ->
  Std.Supervisor.stop t.runtime;
  Sqlx_backend.shutdown t.db

let routes = fun t -> Routes.routes (Routes.sqlx_store t.db)
