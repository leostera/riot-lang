open Std
open Std.Collections
open Std.Sync


type config =
  | Config : {
      driver : (module Sqlx_driver.Driver.Intf with type config = 'config);
      driver_config : 'config;
      min_connections : int;
      max_connections : int;
      acquire_timeout : Time.Duration.t;
      idle_timeout : Time.Duration.t;
      max_lifetime : Time.Duration.t option;
    }
      -> config

type t = { config : config; supervisor : Pid.t }

type connection_state =
  | Available of Connection.t
  | InUse of Connection.t * Pid.t * Time.Instant.t

type pool_msg =
  | Acquire of Pid.t
  | Release of Connection.t * Pid.t
  | HealthCheck
  | GetStats of Pid.t
  | Shutdown

type Message.t += PoolMsg of pool_msg

type pool_response =
  | ConnectionAcquired of Connection.t
  | AcquireError of string
  | Stats of
      [ `Total of int | `Available of int | `InUse of int | `Waiting of int ]
      list

type Message.t += PoolResponse of pool_response

type pool_state = {
  connections : connection_state list Cell.t;
  waiting : Pid.t Queue.t;
  config : config;
  min_connections : int;
  max_connections : int;
  idle_timeout : Time.Duration.t;
  max_lifetime : Time.Duration.t option;
}

let spawn_connection (Config { driver; driver_config; _ }) =
  Connection.create (Connection.Config { driver; config = driver_config })

let find_available connections =
  List.find_opt (function Available _ -> true | _ -> false) (Cell.get connections)

let mark_in_use connections conn requester =
  Cell.set connections
    (List.map
      (function
        | Available c when Connection.id c = Connection.id conn ->
            InUse (c, requester, Time.Instant.now ())
        | other -> other)
      (Cell.get connections))

let mark_available connections conn =
  Cell.set connections
    (List.map
      (function
        | InUse (c, _, _) when Connection.id c = Connection.id conn ->
            Available c
        | other -> other)
      (Cell.get connections))

let handle_acquire state requester =
  match find_available state.connections with
  | Some (Available conn) ->
      mark_in_use state.connections conn requester;
      send requester (PoolResponse (ConnectionAcquired conn))
  | _ ->
      let total = List.length (Cell.get state.connections) in
      if total < state.max_connections then
        match spawn_connection state.config with
        | Ok conn ->
            Cell.set state.connections
              (InUse (conn, requester, Time.Instant.now ())
              :: Cell.get state.connections);
            send requester (PoolResponse (ConnectionAcquired conn))
        | Error e -> send requester (PoolResponse (AcquireError e))
      else Queue.push state.waiting requester

let handle_release state conn =
  mark_available state.connections conn;
  match Queue.pop state.waiting with
  | Some requester ->
      mark_in_use state.connections conn requester;
      send requester (PoolResponse (ConnectionAcquired conn))
  | None -> ()

let check_connections state =
  let now = Time.Instant.now () in
  let updated =
    List.filter_map
      (function
        | Available conn ->
            let age =
              Time.Instant.duration_since
                ~earlier:(Connection.created_at conn)
                now
            in
            let idle =
              Time.Instant.duration_since
                ~earlier:(Connection.last_used conn)
                now
            in
            if Time.Duration.compare idle state.idle_timeout > 0 then (
              Connection.close conn;
              None)
            else if Option.is_some state.max_lifetime then
              let max_life = Option.unwrap state.max_lifetime in
              if Time.Duration.compare age max_life > 0 then (
                Connection.close conn;
                None)
              else Some (Available conn)
            else Some (Available conn)
        | InUse _ as conn -> Some conn)
      (Cell.get state.connections)
  in
  Cell.set state.connections updated;

  let total = List.length (Cell.get state.connections) in
  if total < state.min_connections then
    for _ = 1 to state.min_connections - total do
      match spawn_connection state.config with
      | Ok conn -> Cell.set state.connections (Available conn :: Cell.get state.connections)
      | Error _ -> ()
    done

let get_stats state =
  let total = List.length (Cell.get state.connections) in
  let available =
    List.fold_left
      (fun acc -> function Available _ -> acc + 1 | _ -> acc)
      0 (Cell.get state.connections)
  in
  let in_use = total - available in
  let waiting = Queue.len state.waiting in
  [ `Total total; `Available available; `InUse in_use; `Waiting waiting ]

let pool_supervisor
    (Config { min_connections; max_connections; idle_timeout; max_lifetime; _ }
     as config) =
  let state =
    {
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
    | Error e -> Log.error ("Failed to create initial connection: " ^ e)
  done;

  let rec loop () =
    let selector msg =
      match msg with PoolMsg msg -> `select msg | _ -> `skip
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
        List.iter
          (function
            | Available conn | InUse (conn, _, _) -> Connection.close conn)
          (Cell.get state.connections);
        ()
  in
  loop ()

let create (Config { min_connections; max_connections; _ } as config) =
  if min_connections < 0 || max_connections < min_connections then
    Error "Invalid pool configuration"
  else
    let supervisor =
      spawn (fun () ->
          pool_supervisor config;
          Ok ())
    in
    Ok { config; supervisor }

let acquire t =
  send t.supervisor (PoolMsg (Acquire (self ())));
  (* TODO(@leostera): use receive ?timeout once available *)
  let selector msg =
    match msg with
    | PoolResponse (ConnectionAcquired conn) -> `select (Ok conn)
    | PoolResponse (AcquireError e) -> `select (Error e)
    | _ -> `skip
  in
  receive ~selector ()

let release t conn = send t.supervisor (PoolMsg (Release (conn, self ())))

let with_connection t f =
  match acquire t with
  | Error _ as err -> err
  | Ok conn ->
      let result = f conn in
      release t conn;
      result

let shutdown t = send t.supervisor (PoolMsg Shutdown)

let stats t =
  send t.supervisor (PoolMsg (GetStats (self ())));
  let selector msg =
    match msg with PoolResponse (Stats s) -> `select s | _ -> `skip
  in
  receive ~selector ()
