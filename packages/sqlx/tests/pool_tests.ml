open Std
open Sqlx

module Queue = Collections.Queue

let test_rng =
  Random.Rng.standard ~seed:"sqlx-pool-tests" ()
  |> Result.expect ~msg:"failed to create sqlx pool test rng"

module MockDriver: Driver.Intf with type config = unit = struct
  type config = unit

  type connection = { id: int }

  type statement = { sql: string }

  type result_set = {
    rows: Row.t Queue.t;
    rows_affected: int;
  }

  type error = string

  let name = "Mock"

  let error_to_string = fun error -> error

  let error_to_json = fun error -> Data.Json.String error

  let connect = fun () ->
    Ok {
      id =
        Random.int ~rng:test_rng 1_000
        |> Result.expect ~msg:"mock connection id";
    }

  let close = fun _ -> ()

  let ping = fun _ -> true

  let prepare = fun _conn sql -> Ok { sql }

  let execute = fun _stmt _params -> Ok { rows = Queue.create (); rows_affected = 0 }

  let fetch_row = fun result -> Queue.pop result.rows

  let rows_affected = fun result -> result.rows_affected

  let begin_transaction = fun _ -> Ok ()

  let commit = fun _ -> Ok ()

  let rollback = fun _ -> Ok ()

  let set_isolation_level = fun _ _ -> Ok ()
end

module ClosingDriver: Driver.Intf with type config = unit = struct
  type config = unit

  type connection = {
    id: int;
    mutable closed: bool;
  }

  type statement = {
    conn: connection;
    sql: string;
  }

  type result_set = {
    rows: Row.t Queue.t;
    rows_affected: int;
  }

  type error = string

  let name = "Closing"

  let error_to_string = fun error -> error

  let error_to_json = fun error -> Data.Json.String error

  let connect = fun () ->
    Ok {
      id =
        Random.int ~rng:test_rng 1_000
        |> Result.expect ~msg:"closing driver connection id";
      closed = false;
    }

  let close = fun conn -> conn.closed <- true

  let ping = fun conn -> not conn.closed

  let prepare = fun conn sql ->
    if conn.closed then
      Error "closed"
    else
      Ok { conn; sql }

  let execute = fun stmt _params ->
    if stmt.conn.closed then
      Error "closed"
    else if stmt.sql = "close" then (
      stmt.conn.closed <- true;
      Error "closed during execute"
    ) else
      Ok { rows = Queue.create (); rows_affected = 1 }

  let fetch_row = fun result -> Queue.pop result.rows

  let rows_affected = fun result -> result.rows_affected

  let begin_transaction = fun _ -> Ok ()

  let commit = fun _ -> Ok ()

  let rollback = fun _ -> Ok ()

  let set_isolation_level = fun _ _ -> Ok ()
end

module RaisingDriver: Driver.Intf with type config = unit = struct
  type config = unit

  type connection = { mutable closed: bool }

  type statement = {
    conn: connection;
  }

  type result_set = {
    rows: Row.t Queue.t;
    rows_affected: int;
  }

  type error = string

  let name = "Raising"

  let error_to_string = fun error -> error

  let error_to_json = fun error -> Data.Json.String error

  let connect = fun () -> Ok { closed = false }

  let close = fun conn -> conn.closed <- true

  let ping = fun conn -> not conn.closed

  let prepare = fun conn _sql -> Ok { conn }

  let execute = fun _stmt _params -> raise (Failure "driver exploded")

  let fetch_row = fun result -> Queue.pop result.rows

  let rows_affected = fun result -> result.rows_affected

  let begin_transaction = fun _ -> Ok ()

  let commit = fun _ -> Ok ()

  let rollback = fun _ -> Ok ()

  let set_isolation_level = fun _ _ -> Ok ()
end

let test_pool_creation = fun _ctx ->
  let config = Pool.Config {
    driver = (module MockDriver);
    driver_config = ();
    min_connections = 2;
    max_connections = 5;
    acquire_timeout = Time.Duration.from_secs 5;
    idle_timeout = Time.Duration.from_mins 5;
    max_lifetime = Some (Time.Duration.from_hours 1);
  }
  in
  match Pool.create config with
  | Error error -> Error (Sqlx.show_error (Sqlx.PoolError (Sqlx.Pool.ConnectionError error)))
  | Ok pool ->
      let stats = Pool.stats pool in
      Test.assert_true (List.contains stats ~value:(Pool.TotalConnections 2));
      Test.assert_true (List.contains stats ~value:(Pool.AvailableConnections 2));
      Pool.shutdown pool;
      Ok ()

let test_pool_discards_closed_connection_on_release = fun _ctx ->
  let config = Pool.Config {
    driver = (module ClosingDriver);
    driver_config = ();
    min_connections = 1;
    max_connections = 1;
    acquire_timeout = Time.Duration.from_secs 5;
    idle_timeout = Time.Duration.from_mins 5;
    max_lifetime = Some (Time.Duration.from_hours 1);
  }
  in
  match Pool.create config with
  | Error error -> Error (Sqlx.show_error (Sqlx.PoolError (Sqlx.Pool.ConnectionError error)))
  | Ok pool ->
      (
        match Pool.with_connection pool (fun conn -> Connection.execute conn "close" []) with
        | Ok _ -> Error "expected closing driver to fail"
        | Error _ -> Ok ()
      )
      |> Result.and_then
        ~fn:(fun () ->
          let stats = Pool.stats pool in
          Test.assert_true (List.contains stats ~value:(Pool.TotalConnections 0));
          match Pool.with_connection pool (fun conn -> Connection.execute conn "ok" []) with
          | Error error -> Error (Sqlx.show_error (Sqlx.PoolError error))
          | Ok rows ->
              Test.assert_equal ~expected:1 ~actual:rows;
              let stats = Pool.stats pool in
              Test.assert_true (List.contains stats ~value:(Pool.TotalConnections 1));
              Pool.shutdown pool;
              Ok ())

let test_pool_discards_connection_after_driver_exception = fun _ctx ->
  let config = Pool.Config {
    driver = (module RaisingDriver);
    driver_config = ();
    min_connections = 1;
    max_connections = 1;
    acquire_timeout = Time.Duration.from_secs 5;
    idle_timeout = Time.Duration.from_mins 5;
    max_lifetime = Some (Time.Duration.from_hours 1);
  }
  in
  match Pool.create config with
  | Error error -> Error (Sqlx.show_error (Sqlx.PoolError (Sqlx.Pool.ConnectionError error)))
  | Ok pool ->
      (
        match Pool.with_connection pool (fun conn -> Connection.execute conn "raise" []) with
        | Ok _ -> Error "expected raising driver to fail"
        | Error (Pool.ConnectionError (Connection.RuntimeError (Connection.RaisedException message))) ->
            Test.assert_equal ~expected:"Failure: driver exploded" ~actual:message;
            Ok ()
        | Error error -> Error (Sqlx.show_error (Sqlx.PoolError error))
      )
      |> Result.and_then
        ~fn:(fun () ->
          let stats = Pool.stats pool in
          Test.assert_true (List.contains stats ~value:(Pool.TotalConnections 0));
          Pool.shutdown pool;
          Ok ())

let tests =
  Test.[
    case "pool creation" test_pool_creation;
    case
      "pool discards closed connection on release"
      test_pool_discards_closed_connection_on_release;
    case
      "pool discards connection after driver exception"
      test_pool_discards_connection_after_driver_exception;
  ]

let main ~args = Test.Cli.main ~name:"sqlx_pool_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
