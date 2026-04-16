open Std

module Waiters = Collections.Queue

type t = {
  init_lock: Kernel.Sync.Mutex.t;
  mutable server: Pid.t option;
}

type request_id = int

type owner = {
  pid: Pid.t;
  monitor: Actor.Monitor.t;
}

type request =
  | Acquire of {
      reply_to: Pid.t;
      request_id: request_id;
    }
  | Release of {
      reply_to: Pid.t;
      request_id: request_id;
    }

type Message.t +=
  | Riot_build_actor_mutex_request of request
  | Riot_build_actor_mutex_acquired of { request_id: request_id }
  | Riot_build_actor_mutex_released of { request_id: request_id }
  | Riot_build_actor_mutex_failed of {
      request_id: request_id;
      reason: string;
    }

let request_ids = Kernel.Sync.Atomic.make 0

let next_request_id = fun () ->
  Kernel.Sync.Atomic.fetch_and_add request_ids 1 + 1

let with_init_lock = fun t f ->
  Kernel.Sync.Mutex.lock t.init_lock;
  try
    let result = f () in
    Kernel.Sync.Mutex.unlock t.init_lock;
    result
  with
  | exn ->
      Kernel.Sync.Mutex.unlock t.init_lock;
      raise exn

module Server = struct
  type state = {
    mutable owner: owner option;
    waiters: (Pid.t * request_id) Waiters.t;
  }

  let grant = fun state pid request_id ->
    let monitor = Actor.monitor pid in
    state.owner <- Some { pid; monitor };
    send pid (Riot_build_actor_mutex_acquired { request_id })

  let rec grant_next = fun state ->
    match Waiters.pop state.waiters with
    | None ->
        state.owner <- None
    | Some (pid, request_id) ->
        grant state pid request_id

  let fail = fun reply_to request_id reason ->
    send reply_to (Riot_build_actor_mutex_failed { request_id; reason })

  let release = fun state reply_to request_id ->
    match state.owner with
    | Some owner when Pid.equal owner.pid reply_to ->
        Actor.demonitor owner.monitor;
        state.owner <- None;
        send reply_to (Riot_build_actor_mutex_released { request_id });
        grant_next state
    | Some _ ->
        fail reply_to request_id "actor mutex unlock by non-owner"
    | None ->
        fail reply_to request_id "actor mutex unlock while unlocked"

  let release_owner_on_exit = fun state monitor_ref pid ->
    match state.owner with
    | Some owner when owner.monitor = monitor_ref && Pid.equal owner.pid pid ->
        state.owner <- None;
        grant_next state
    | _ -> ()

  let rec loop = fun state ->
    let selector msg =
      match msg with
      | Riot_build_actor_mutex_request request -> `select (`request request)
      | Actor.DOWN { ref; pid; _ } -> `select (`down (ref, pid))
      | _ -> `skip
    in
    match receive ~selector () with
    | `request (Acquire { reply_to; request_id }) ->
        (
          match state.owner with
          | None -> grant state reply_to request_id
          | Some _ -> Waiters.push state.waiters ~value:(reply_to, request_id)
        );
        loop state
    | `request (Release { reply_to; request_id }) ->
        release state reply_to request_id;
        loop state
    | `down (monitor_ref, pid) ->
        release_owner_on_exit state monitor_ref pid;
        loop state

  let start = fun () ->
    spawn
      (fun () ->
        loop { owner = None; waiters = Waiters.create () })
end

let create = fun () ->
  {
    init_lock = Kernel.Sync.Mutex.create ();
    server = None;
  }

let ensure_server = fun t ->
  with_init_lock t
    (fun () ->
      match t.server with
      | Some server -> server
      | None ->
          let server = Server.start () in
          t.server <- Some server;
          server)

let await = fun request_id expected ->
  let selector msg =
    match msg with
    | Riot_build_actor_mutex_acquired { request_id = got }
      when expected = `acquired && Int.equal got request_id -> `select (Ok ())
    | Riot_build_actor_mutex_released { request_id = got }
      when expected = `released && Int.equal got request_id -> `select (Ok ())
    | Riot_build_actor_mutex_failed { request_id = got; reason }
      when Int.equal got request_id -> `select (Error reason)
    | _ -> `skip
  in
  receive ~selector ()

let lock = fun t ->
  let server = ensure_server t in
  let request_id = next_request_id () in
  send server (Riot_build_actor_mutex_request (Acquire { reply_to = self (); request_id }));
  match await request_id `acquired with
  | Ok () -> ()
  | Error reason -> raise (Failure reason)

let unlock = fun t ->
  let server = ensure_server t in
  let request_id = next_request_id () in
  send server (Riot_build_actor_mutex_request (Release { reply_to = self (); request_id }));
  match await request_id `released with
  | Ok () -> ()
  | Error reason -> raise (Failure reason)
