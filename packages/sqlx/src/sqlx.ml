open Std

module Ser = Serde.Ser

module ProtocolError = struct
  type t =
    | P: {
        error: 'err;
        serializer: 'err Ser.t;
        to_string: 'err -> string;
      } -> t

  let serializer = {
    Ser.run = (fun backend state (P { error; serializer; _ }) -> serializer.run backend state error);
  }

  let to_string = fun (P { error; to_string; _ }) -> to_string error

  let make = fun error ~serializer ~to_string -> P { error; serializer; to_string }
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

module Config = struct
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

  let default = {
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
module Migrate = Migrate

let connect = fun ?(config = Config.default) ~driver driver_config ->
  let pool_config = Pool.Config {
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
  | Error conn_err -> Error (PoolError (Pool.ConnectionError conn_err))

let query = fun pool sql params ->
  match Pool.with_connection pool (fun conn -> Connection.query conn sql params) with
  | Ok cursor -> Ok cursor
  | Error pool_err -> Error (PoolError pool_err)

let exec = fun pool sql params ->
  match Pool.with_connection pool (fun conn -> Connection.execute conn sql params) with
  | Ok rows -> Ok rows
  | Error pool_err -> Error (PoolError pool_err)

let migrate = fun ?config ?source pool () ->
  let source =
    match source with
    | Some source -> source
    | None -> Migrate.Source.from_directory (Path.v "migrations")
  in
  match Migrate.run ?config pool source with
  | Ok _report -> Ok ()
  | Error error -> Error error

let show_pool_error = fun error ->
  match error with
  | Pool.Exhausted { waiting; max_connections; timeout } ->
      "Pool exhausted: "
      ^ Int.to_string waiting
      ^ " waiting, max "
      ^ Int.to_string max_connections
      ^ " connections, timeout "
      ^ (Time.Duration.to_secs_string timeout)
  | Pool.ConnectionError error -> "Connection error: " ^ Connection.error_to_string error
  | Pool.Timeout duration -> "Pool timeout after " ^ Time.Duration.to_secs_string duration

let with_transaction = fun pool f ->
  match Pool.with_connection pool (fun conn -> Transaction.with_transaction conn f) with
  | Ok v -> Ok v
  | Error pool_err -> Error (PoolError pool_err)

let shutdown = fun pool -> Pool.shutdown pool

let show_error = fun error ->
  match error with
  | PoolError pool_err -> show_pool_error pool_err
  | InvalidValue {
      field;
      value;
      expected_type;
      reason;
    } ->
      "Invalid value for '" ^ field ^ "': got '" ^ value ^ "', expected " ^ expected_type ^ (
        match reason with
        | Some r -> " (" ^ r ^ ")"
        | None -> ""
      )
  | Timeout { operation; duration } ->
      let op_str =
        match operation with
        | Acquire -> "acquire"
        | Query -> "query"
        | Transaction -> "transaction"
      in
      "Timeout during " ^ op_str ^ " after " ^ (Time.Duration.to_secs_string duration)
