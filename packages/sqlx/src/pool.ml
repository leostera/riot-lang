open Std
open Std.Collections
open Std.Sync

type error =
  | Exhausted of { waiting: int; max_connections: int; timeout: Time.Duration.t }
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

type t = {
  config: config;
  supervisor: Pid.t;
}

type connection_state =
  | Available of Connection.t
  | InUse of Connection.t * Pid.t * Time.Instant.t

type pool_msg =
  | Acquire of Pid.t
  | Release of Connection.t * Pid.t
  | HealthCheck
  | GetStats of Pid.t
  | Shutdown

type Message.t +=
  PoolMsg of pool_msg

type pool_response =
  | ConnectionAcquired of Connection.t
  | AcquireError of error
  | Stats of ([
    `Total of int
    | `Available of int
    | `InUse of int
    | `Waiting of int
  ]) list

type Message.t +=
  PoolResponse of pool_response

type pool_state = {
  connections: connection_state list Cell.t;
  waiting: Pid.t Queue.t;
  config: config;
  min_connections: int;
  max_connections: int;
  idle_timeout: Time.Duration.t;
  max_lifetime: Time.Duration.t option;
}

let spawn_connection = fun (Config { driver; driver_config; _ }) ->
  Connection.create (Connection.Config { driver; config = driver_config })

let find_available = fun connections ->
  List.find (Cell.get connections)
    ~fn:(
      function
      | Available _ -> true
      | _ -> false
    )

let mark_in_use = fun connections conn requester ->
  Cell.set connections
    (
      List.map (Cell.get connections)
        ~fn:(
          function
          | Available c when Connection.id c = Connection.id conn -> InUse (
            c,
            requester,
            Time.Instant.now ()
          )
          | other -> other
        )
    )

let mark_available = fun connections conn ->
  Cell.set connections
    (
      List.map (Cell.get connections)
        ~fn:(
          function
          | InUse (c, _, _) when Connection.id c = Connection.id conn -> Available c
          | other -> other
        )
    )

let handle_acquire = fun state requester ->
  match find_available state.connections with
  | Some (Available conn) ->
      mark_in_use state.connections conn requester;
      send requester (PoolResponse (ConnectionAcquired conn))
  | _ ->
      let total = List.length (Cell.get state.connections) in
      if total < state.max_connections then
        match spawn_connection state.config with
        | Ok conn ->
            Cell.set
              state.connections
              (InUse (conn, requester, Time.Instant.now ()) :: Cell.get state.connections);
            send requester (PoolResponse (ConnectionAcquired conn))
        | Error conn_err -> send requester (PoolResponse (AcquireError (ConnectionError conn_err)))
      else
        Queue.push state.waiting ~value:requester

let handle_release = fun state conn ->
  mark_available state.connections conn;
  match Queue.pop state.waiting with
  | Some requester ->
      mark_in_use state.connections conn requester;
      send requester (PoolResponse (ConnectionAcquired conn))
  | None -> ()

let check_connections = fun state ->
  let now = Time.Instant.now () in
  let updated =
    List.filter_map (Cell.get state.connections)
      ~fn:(
        function
        | Available conn ->
            let age = Time.Instant.duration_since ~earlier:(Connection.created_at conn) now in
            let idle = Time.Instant.duration_since ~earlier:(Connection.last_used conn) now in
            if Time.Duration.compare idle state.idle_timeout > 0 then
              (
                Connection.close conn;
                None
              )
            else if Option.is_some state.max_lifetime then
              let max_life = Option.unwrap state.max_lifetime in
              if Time.Duration.compare age max_life > 0 then
                (
                  Connection.close conn;
                  None
                )
              else
                Some (Available conn)
            else
              Some (Available conn)
        | InUse _ as conn -> Some conn
      )
  in
  Cell.set state.connections updated;
  let total = List.length (Cell.get state.connections) in
  if total < state.min_connections then
    for _ = 1 to state.min_connections - total do
      match spawn_connection state.config with
      | Ok conn -> Cell.set state.connections (Available conn :: Cell.get state.connections)
      | Error _ -> ()
    done

let get_stats = fun state ->
  let total = List.length (Cell.get state.connections) in
  let available =
    List.fold_left (Cell.get state.connections) ~init:0
      ~fn:(fun acc ->
        function
        | Available _ -> acc + 1
        | _ -> acc)
  in
  let in_use = total - available in
  let waiting = Queue.length state.waiting in
  [ `Total total; `Available available; `InUse in_use; `Waiting waiting ]

let pool_supervisor = fun
  (Config {
    min_connections;
    max_connections;
    idle_timeout;
    max_lifetime;
    _
  } as config) ->
  let state = {
    connections = Cell.create [];
    waiting = Queue.create ();
    config;
    min_connections;
    max_connections;
    idle_timeout;
    max_lifetime;
  }
  in
  for _ = 1 to min_connections do
    match spawn_connection config with
    | Ok conn -> Cell.set state.connections (Available conn :: Cell.get state.connections)
    | Error conn_err ->
        let (Connection.DriverError { error; to_string; _ }) = conn_err in
        Log.error ("Failed to create initial connection: " ^ to_string error)
  done;
  let rec loop () =
    let selector msg =
      match msg with
      | PoolMsg msg -> `select msg
      | _ -> `skip
    in
    match receive ~selector () with
    | Acquire requester ->
        handle_acquire state requester;
        loop ()
    | Release (conn, _releaser) ->
        handle_release state conn;
        loop ()
    | HealthCheck ->
        check_connections state;
        loop ()
    | GetStats reply_to ->
        let stats = get_stats state in
        send reply_to (PoolResponse (Stats stats));
        loop ()
    | Shutdown ->
        List.for_each (Cell.get state.connections)
          ~fn:(
            function
            | Available conn
            | InUse (conn, _, _) -> Connection.close conn
          );
        ()
  in
  loop ()

let create = fun (Config { min_connections; max_connections; _ } as config) ->
  if min_connections < 0 || max_connections < min_connections then
    Error (Connection.DriverError {
      error = "Invalid pool configuration";
      to_string = (fun s -> s);
      to_json = (fun s -> Data.Json.string s)
    })
  else
    (* Try to create at least one connection to validate driver config *)
    match spawn_connection config with
    | Error conn_err -> Error conn_err
    | Ok _test_conn ->
        let supervisor =
          spawn
            (fun () ->
              pool_supervisor config;
              Ok ())
        in
        Ok { config; supervisor }

let acquire = fun t ->
  send t.supervisor (PoolMsg (Acquire (self ())));
  (* TODO(@leostera): use receive ?timeout once available *)
  let selector msg =
    match msg with
    | PoolResponse (ConnectionAcquired conn) -> `select (Ok conn)
    | PoolResponse (AcquireError e) -> `select (Error e)
    | _ -> `skip
  in
  receive ~selector ()

let release = fun t conn -> send t.supervisor (PoolMsg (Release (conn, self ())))

let with_connection = fun t f ->
  match acquire t with
  | Error _ as err -> err
  | Ok conn ->
      let result =
        match f conn with
        | Ok v -> Ok v
        | Error conn_err -> Error (ConnectionError conn_err)
      in
      release t conn;
      result

let shutdown = fun t -> send t.supervisor (PoolMsg Shutdown)

let stats = fun t ->
  send t.supervisor (PoolMsg (GetStats (self ())));
  let selector msg =
    match msg with
    | PoolResponse (Stats s) -> `select s
    | _ -> `skip
  in
  receive ~selector ()
