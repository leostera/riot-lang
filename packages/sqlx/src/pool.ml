open Std
open Std.Collections
open Std.Sync

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

type t = {
  config: config;
  supervisor: Pid.t;
}

type connection_state =
  | Available of Connection.t
  | InUse of Connection.t * Pid.t * int * Time.Instant.t

type stat =
  | TotalConnections of int
  | AvailableConnections of int
  | InUseConnections of int
  | WaitingRequests of int

type pool_msg =
  | Acquire of Pid.t
  | Release of Connection.t * Pid.t * int
  | HealthCheck
  | GetStats of Pid.t
  | Shutdown

type Message.t +=
  | PoolMsg of pool_msg

type pool_response =
  | ConnectionAcquired of Connection.t
  | AcquireError of error
  | Stats of stat list

type Message.t +=
  | PoolResponse of pool_response

type pool_state = {
  connections: connection_state list Cell.t;
  waiting: Pid.t Queue.t;
  config: config;
  min_connections: int;
  max_connections: int;
  idle_timeout: Time.Duration.t;
  max_lifetime: Time.Duration.t option;
  next_lease: int Cell.t;
}

let exception_to_string = fun caught ->
  match caught with
  | Failure message -> "Failure: " ^ message
  | Invalid_argument message -> "Invalid_argument: " ^ message
  | Not_found -> "Not_found"
  | End_of_file -> "End_of_file"
  | Division_by_zero -> "Division_by_zero"
  | exn -> Exception.to_string exn

let spawn_connection = fun (Config { driver; driver_config; _ }) ->
  Connection.create
    (Connection.Config { driver; config = driver_config })

let find_available = fun connections ->
  let checked =
    List.filter_map
      (Cell.get connections)
      ~fn:(fun state ->
        match state with
        | Available conn when not (Connection.ping conn) ->
            Connection.close conn;
            None
        | state -> Some state)
  in
  Cell.set connections checked;
  List.find
    checked
    ~fn:(fun state ->
      match state with
      | Available _ -> true
      | _ -> false)

let mark_in_use = fun connections conn requester lease ->
  Connection.set_pool_lease conn lease;
  Cell.set
    connections
    (
      List.map
        (Cell.get connections)
        ~fn:(fun conn_state ->
          match conn_state with
          | Available c when Connection.id c = Connection.id conn ->
              InUse (c, requester, lease, Time.Instant.now ())
          | other -> other)
    )

let next_lease = fun state ->
  let lease = Cell.get state.next_lease + 1 in
  Cell.set state.next_lease lease;
  lease

let release_connection = fun state conn releaser lease ->
  let released = ref false in
  let found = ref false in
  Cell.set
    state.connections
    (
      List.filter_map
        (Cell.get state.connections)
        ~fn:(fun conn_state ->
          match conn_state with
          | InUse (c, owner, current_lease, _) when Connection.id c = Connection.id conn ->
              found := true;
              if Pid.equal owner releaser && current_lease = lease then (
                released := true;
                if Connection.ping c then
                  Some (Available c)
                else (
                  Connection.close c;
                  None
                )
              ) else (
                Log.warn
                  ("sqlx pool ignored stale release for connection "
                  ^ Connection.id c
                  ^ " lease="
                  ^ Int.to_string lease
                  ^ " current="
                  ^ Int.to_string current_lease);
                Some conn_state
              )
          | Available c when Connection.id c = Connection.id conn ->
              found := true;
              Log.warn
                ("sqlx pool ignored release for available connection "
                ^ Connection.id c
                ^ " lease="
                ^ Int.to_string lease);
              Some conn_state
          | other -> Some other)
    );
  if not !found then
    Log.warn
      ("sqlx pool ignored release for unknown connection "
      ^ Connection.id conn
      ^ " lease="
      ^ Int.to_string lease);
  !released

let handle_acquire = fun state requester ->
  match find_available state.connections with
  | Some (Available conn) ->
      let lease = next_lease state in
      mark_in_use state.connections conn requester lease;
      send requester (PoolResponse (ConnectionAcquired conn))
  | _ ->
      let total = List.length (Cell.get state.connections) in
      if total < state.max_connections then
        match spawn_connection state.config with
        | Ok conn ->
            let lease = next_lease state in
            Connection.set_pool_lease conn lease;
            Cell.set
              state.connections
              (InUse (conn, requester, lease, Time.Instant.now ()) :: Cell.get state.connections);
            send requester (PoolResponse (ConnectionAcquired conn))
        | Error conn_err -> send requester (PoolResponse (AcquireError (ConnectionError conn_err)))
      else
        Queue.push state.waiting ~value:requester

let handle_release = fun state conn releaser lease ->
  if release_connection state conn releaser lease then
    match Queue.pop state.waiting with
    | Some requester -> handle_acquire state requester
    | None -> ()

let check_connections = fun state ->
  let now = Time.Instant.now () in
  let updated =
    List.filter_map
      (Cell.get state.connections)
      ~fn:(fun conn_state ->
        match conn_state with
        | Available conn ->
            let age = Time.Instant.duration_since ~earlier:(Connection.created_at conn) now in
            let idle = Time.Instant.duration_since ~earlier:(Connection.last_used conn) now in
            if not (Connection.ping conn) then (
              Connection.close conn;
              None
            ) else if Time.Duration.compare idle state.idle_timeout = Order.GT then (
              Connection.close conn;
              None
            ) else if Option.is_some state.max_lifetime then
              let max_life = Option.unwrap state.max_lifetime in
              if Time.Duration.compare age max_life = Order.GT then (
                Connection.close conn;
                None
              ) else
                Some (Available conn)
            else
              Some (Available conn)
        | InUse _ as conn -> Some conn)
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
    List.fold_left
      (Cell.get state.connections)
      ~init:0
      ~fn:(fun acc ->
        fun state ->
          match state with
          | Available _ -> acc + 1
          | _ -> acc)
  in
  let in_use = total - available in
  let waiting = Queue.length state.waiting in
  [
    TotalConnections total;
    AvailableConnections available;
    InUseConnections in_use;
    WaitingRequests waiting;
  ]

let pool_supervisor = fun
  (Config {
     min_connections;
     max_connections;
     idle_timeout;
     max_lifetime;
     _;
   } as config) ->
  let state = {
    connections = Cell.create [];
    waiting = Queue.create ();
    config;
    min_connections;
    max_connections;
    idle_timeout;
    max_lifetime;
    next_lease = Cell.create 0;
  }
  in
  for _ = 1 to min_connections do
    match spawn_connection config with
    | Ok conn -> Cell.set state.connections (Available conn :: Cell.get state.connections)
    | Error conn_err ->
        Log.error ("Failed to create initial connection: " ^ Connection.error_to_string conn_err)
  done;
  let rec loop () =
    let selector msg =
      match msg with
      | PoolMsg msg -> Select msg
      | _ -> Skip
    in
    try
      match receive ~selector () with
      | Acquire requester ->
          handle_acquire state requester;
          loop ()
      | Release (conn, releaser, lease) ->
          handle_release state conn releaser lease;
          loop ()
      | HealthCheck ->
          check_connections state;
          loop ()
      | GetStats reply_to ->
          let stats = get_stats state in
          send reply_to (PoolResponse (Stats stats));
          loop ()
      | Shutdown ->
          List.for_each
            (Cell.get state.connections)
            ~fn:(fun (Available conn | InUse (conn, _, _, _)) -> Connection.close conn);
          ()
    with
    | exn ->
        Log.error ("sqlx pool supervisor raised: " ^ exception_to_string exn);
        sleep (Time.Duration.from_secs 1);
        loop ()
  in
  loop ()

let create = fun (Config { min_connections; max_connections; _ } as config) ->
  if min_connections < 0 || max_connections < min_connections then
    Error (Connection.RuntimeError (Connection.InvalidConfiguration "invalid pool configuration"))
  else
    (* Try to create at least one connection to validate driver config *)
    match spawn_connection config with
    | Error conn_err -> Error conn_err
    | Ok test_conn ->
        Connection.close test_conn;
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
    | PoolResponse (ConnectionAcquired conn) -> Select (Ok conn)
    | PoolResponse (AcquireError e) -> Select (Error e)
    | _ -> Skip
  in
  receive ~selector ()

let release = fun t conn ->
  send
    t.supervisor
    (PoolMsg (Release (conn, self (), Connection.pool_lease conn)))

let with_connection = fun t f ->
  match acquire t with
  | Error _ as err -> err
  | Ok conn ->
      try
        let result =
          match f conn with
          | Ok v -> Ok v
          | Error conn_err ->
              Connection.close conn;
              Error (ConnectionError conn_err)
        in
        release t conn;
        result
      with
      | exn ->
          Connection.close conn;
          release t conn;
          raise exn

let shutdown = fun t -> send t.supervisor (PoolMsg Shutdown)

let stats = fun t ->
  send t.supervisor (PoolMsg (GetStats (self ())));
  let selector msg =
    match msg with
    | PoolResponse (Stats s) -> Select s
    | _ -> Skip
  in
  receive ~selector ()
