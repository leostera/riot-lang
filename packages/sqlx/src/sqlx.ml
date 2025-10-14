open Std

type error =
  | Connection_failed of string
  | Query_failed of string
  | Pool_exhausted
  | Invalid_value of string
  | Driver_error of string

module Config = struct
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

  let default =
    {
      pool_size = 10;
      max_idle_time = Time.Duration.from_mins 10;
      acquire_timeout = Time.Duration.from_secs 30;
      idle_check_interval = Time.Duration.from_mins 1;
      max_lifetime = Some (Time.Duration.from_hours 1);
      auto_commit = true;
      isolation_level = None;
      query_timeout = None;
      log_queries = false;
      log_slow_queries = None;
    }
end

module Connection = Connection
module Cursor = Cursor
module Row = Sqlx_driver.Row
module Value = Sqlx_driver.Value
module Transaction = Transaction
module Driver = Sqlx_driver.Driver
module Pool = Pool

let connect ?(config = Config.default) ~driver driver_config =
  let pool_config =
    Pool.Config
      {
        driver;
        driver_config;
        min_connections = max 1 (config.pool_size / 4);
        max_connections = config.pool_size;
        acquire_timeout = config.acquire_timeout;
        idle_timeout = config.max_idle_time;
        max_lifetime = config.max_lifetime;
      }
  in
  match Pool.create pool_config with
  | Ok pool -> Ok pool
  | Error msg -> Error (Connection_failed msg)

let query pool sql params =
  match
    Pool.with_connection pool (fun conn ->
        match Connection.query conn sql params with
        | Ok cursor -> Ok cursor
        | Error msg -> Error msg)
  with
  | Ok cursor -> Ok cursor
  | Error msg -> Error (Query_failed msg)

let exec pool sql params =
  match
    Pool.with_connection pool (fun conn ->
        match Connection.execute conn sql params with
        | Ok rows -> Ok rows
        | Error msg -> Error msg)
  with
  | Ok rows -> Ok rows
  | Error msg -> Error (Query_failed msg)

let with_transaction pool f =
  Pool.with_connection pool (fun conn -> Transaction.with_transaction conn f)

let shutdown pool = Pool.shutdown pool

let show_error = function
  | Connection_failed msg -> Printf.sprintf "Connection failed: %s" msg
  | Query_failed msg -> Printf.sprintf "Query failed: %s" msg
  | Pool_exhausted -> "Connection pool exhausted"
  | Invalid_value msg -> Printf.sprintf "Invalid value: %s" msg
  | Driver_error msg -> Printf.sprintf "Driver error: %s" msg
