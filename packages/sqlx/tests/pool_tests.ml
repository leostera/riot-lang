open Std
open Sqlx

module Queue = Collections.Queue

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
        Random.int 1_000
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
      Test.assert_true (List.contains stats ~value:(`Total 2));
      Test.assert_true (List.contains stats ~value:(`Available 2));
      Pool.shutdown pool;
      Ok ()

let tests = Test.[ case "pool creation" test_pool_creation ]

let main ~args = Test.Cli.main ~name:"sqlx_pool_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
