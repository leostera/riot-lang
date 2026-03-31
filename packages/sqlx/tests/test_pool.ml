open Std
open Sqlx

module MockDriver : Driver.Intf = struct
  type config = unit

  type connection = {
    id : int;
  }

  type statement = {
    sql : string;
  }

  type result_set = {
    data : Row.t list;
  }

  let name = "Mock"

  let connect = fun () -> Ok {id = Random.int 1_000}

  let close = fun _ -> ()

  let ping = fun _ -> true

  let prepare = fun conn sql -> Ok {sql}

  let execute = fun _ _ -> Ok {data = []}

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
  let config =
    Pool.{
      driver = (module MockDriver);
      driver_config = ();
      min_connections = 2;
      max_connections = 5;
      acquire_timeout = Time.Duration.of_sec 5;
      idle_timeout = Time.Duration.of_min 5;
      max_lifetime = Some (Time.Duration.of_hour 1);

    } in
  match Pool.create config with
  | Error e -> Log.error "Failed to create pool: %s" e
  | Ok pool ->
      let stats = Pool.stats pool in
      Log.info "Pool stats: %s"
        (
          String.concat ", "
            (
              List.map
                (
                  function
                  | `Total n -> Printf.sprintf "total=%d" n
                  | `Available n -> Printf.sprintf "available=%d" n
                  | `InUse n -> Printf.sprintf "in_use=%d" n
                  | `Waiting n -> Printf.sprintf "waiting=%d" n
                )
                stats
            )
        );
      Pool.shutdown pool;
      Log.info "Pool creation: OK"

let main = fun () ->
  Log.set_level Log.Debug;
  Log.info "Starting pool tests...";
  let pid =
    spawn
      (fun () ->
        test_pool_creation ();
        Log.info "All pool tests passed!")
  in
  yield ()

let () = main ()
