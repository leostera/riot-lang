open Std
open Sqlx

module MockDriver: Driver.Intf with type config = unit = struct
  type config = unit

  type connection = { id: int }

  type statement = { sql: string }

  type result_set = {
    data: Row.t list;
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

  let prepare = fun conn sql -> Ok { sql }

  let execute = fun _ _ -> Ok { data = [] }

  let fetch_row = fun rs ->
    match rs.data with
    | [] -> None
    | h :: _ -> Some h

  let rows_affected = fun _ -> 0

  let begin_transaction = fun _ -> Ok ()

  let commit = fun _ -> Ok ()

  let rollback = fun _ -> Ok ()

  let set_isolation_level = fun _ _ -> Ok ()
end

let test_pool_creation = fun () ->
  Log.info "Testing pool creation...";
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
  | Error _ -> Log.error "Failed to create pool"
  | Ok pool ->
      let stats = Pool.stats pool in
      Log.info
        (
          "Pool stats: " ^ String.concat
            ", "
            (
              List.map
                ~fn:(fun __tmp1 ->
                  match __tmp1 with
                  | `Total n -> "total=" ^ Int.to_string n
                  | `Available n -> "available=" ^ Int.to_string n
                  | `InUse n -> "in_use=" ^ Int.to_string n
                  | `Waiting n -> "waiting=" ^ Int.to_string n)
                stats
            )
        );
      Pool.shutdown pool;
      Log.info "Pool creation: OK"

let main ~args:_ =
  Log.set_level Log.Debug;
  Log.info "Starting pool tests...";
  let _pid =
    spawn
      (fun () ->
        test_pool_creation ();
        Log.info "All pool tests passed!";
        Ok ())
  in
  yield ();
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
