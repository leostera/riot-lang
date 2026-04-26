open Kernel

module Runtime_atomic = Kernel.Atomic
module Runtime_actor = Runtime.Actor
module Runtime_pid = Runtime.Pid
module Waiters = Collections.Queue

type t = {
  pid: Runtime_pid.t;
}

type request_id = int

type owner = {
  pid: Runtime_pid.t;
  monitor: Runtime_actor.Monitor.t;
}

type request =
  | Acquire of {
      reply_to: Runtime_pid.t;
      request_id: request_id;
    }
  | Try_acquire of {
      reply_to: Runtime_pid.t;
      request_id: request_id;
    }
  | Release of {
      reply_to: Runtime_pid.t;
      request_id: request_id;
    }
  | Suspend of {
      owner: Runtime_pid.t;
      reply_to: Runtime_pid.t;
      request_id: request_id;
    }

type Runtime.Message.t +=
  | Sync_mutex_request of request
  | Sync_mutex_acquired of { request_id: request_id }
  | Sync_mutex_try_result of { request_id: request_id; acquired: bool }
  | Sync_mutex_released of { request_id: request_id }
  | Sync_mutex_suspended of { request_id: request_id }
  | Sync_mutex_failed of { request_id: request_id; reason: string }

type state = {
  mutable owner: owner option;
  waiters: (Runtime_pid.t * request_id) Waiters.t;
}

let request_ids = Runtime_atomic.make 0

let next_request_id = fun () -> Int.succ (Runtime_atomic.fetch_and_add request_ids 1)

let fail = fun reply_to request_id reason ->
  Runtime.send
    reply_to
    (Sync_mutex_failed { request_id; reason })

let grant = fun state pid request_id ->
  let monitor = Runtime_actor.monitor pid in
  state.owner <- Some { pid; monitor };
  Runtime.send pid (Sync_mutex_acquired { request_id })

let rec grant_next = fun state ->
  match Waiters.pop state.waiters with
  | None -> state.owner <- None
  | Some (pid, request_id) -> grant state pid request_id

let release_owner = fun state ->
  match state.owner with
  | None -> ()
  | Some owner ->
      Runtime_actor.demonitor owner.monitor;
      state.owner <- None

let handle_release = fun state reply_to request_id ->
  match state.owner with
  | Some owner when Runtime_pid.equal owner.pid reply_to ->
      release_owner state;
      Runtime.send reply_to (Sync_mutex_released { request_id });
      grant_next state
  | Some _ -> fail reply_to request_id "mutex unlock by non-owner"
  | None -> fail reply_to request_id "mutex unlock while unlocked"

let handle_suspend = fun state owner_pid reply_to request_id ->
  match state.owner with
  | Some owner when Runtime_pid.equal owner.pid owner_pid ->
      release_owner state;
      Runtime.send reply_to (Sync_mutex_suspended { request_id });
      grant_next state
  | Some _ -> fail reply_to request_id "mutex wait without ownership"
  | None -> fail reply_to request_id "mutex wait while unlocked"

let release_owner_on_exit = fun state monitor_ref pid ->
  match state.owner with
  | Some owner when owner.monitor = monitor_ref && Runtime_pid.equal owner.pid pid ->
      state.owner <- None;
      grant_next state
  | _ -> ()

let rec loop = fun state ->
  let selector msg =
    match msg with
    | Sync_mutex_request request -> `select (`request request)
    | Runtime.Actor.DOWN { ref; pid; _ } -> `select (`down (ref, pid))
    | _ -> `skip
  in
  match Runtime.receive ~selector () with
  | `request (Acquire { reply_to; request_id }) ->
      (
        match state.owner with
        | None -> grant state reply_to request_id
        | Some _ -> Waiters.push state.waiters ~value:(reply_to, request_id)
      );
      loop state
  | `request (Try_acquire { reply_to; request_id }) ->
      (
        match state.owner with
        | None ->
            let monitor = Runtime_actor.monitor reply_to in
            state.owner <- Some { pid = reply_to; monitor };
            Runtime.send reply_to (Sync_mutex_try_result { request_id; acquired = true })
        | Some _ -> Runtime.send reply_to (Sync_mutex_try_result { request_id; acquired = false })
      );
      loop state
  | `request (Release { reply_to; request_id }) ->
      handle_release state reply_to request_id;
      loop state
  | `request (Suspend { owner; reply_to; request_id }) ->
      handle_suspend state owner reply_to request_id;
      loop state
  | `down (monitor_ref, pid) ->
      release_owner_on_exit state monitor_ref pid;
      loop state

let create = fun () ->
  {
    pid = Runtime.spawn (fun () -> loop { owner = None; waiters = Waiters.create () });
  }

let await_result = fun request_id expected ->
  let selector msg =
    match msg with
    | Sync_mutex_acquired { request_id = got } when expected = `acquired && Int.equal got request_id ->
        `select (Ok true)
    | Sync_mutex_suspended { request_id = got } when expected = `suspended
    && Int.equal got request_id -> `select (Ok true)
    | Sync_mutex_released { request_id = got } when expected = `released && Int.equal got request_id ->
        `select (Ok true)
    | Sync_mutex_try_result { request_id = got; acquired } when expected = `try_lock
    && Int.equal got request_id -> `select (Ok acquired)
    | Sync_mutex_failed { request_id = got; reason } when Int.equal got request_id ->
        `select (Error reason)
    | _ -> `skip
  in
  Runtime.receive ~selector ()

let lock = fun (t: t) ->
  let request_id = next_request_id () in
  Runtime.send t.pid (Sync_mutex_request (Acquire { reply_to = Runtime.self (); request_id }));
  match await_result request_id `acquired with
  | Ok _ -> ()
  | Error reason -> raise (Failure reason)

let suspend = fun (t: t) ~owner ->
  let request_id = next_request_id () in
  Runtime.send
    t.pid
    (Sync_mutex_request (Suspend { owner; reply_to = Runtime.self (); request_id }));
  match await_result request_id `suspended with
  | Ok _ -> Ok ()
  | Error reason -> Error reason

let unlock = fun (t: t) ->
  let request_id = next_request_id () in
  Runtime.send t.pid (Sync_mutex_request (Release { reply_to = Runtime.self (); request_id }));
  match await_result request_id `released with
  | Ok _ -> ()
  | Error reason -> raise (Failure reason)

let try_lock = fun (t: t) ->
  let request_id = next_request_id () in
  Runtime.send t.pid (Sync_mutex_request (Try_acquire { reply_to = Runtime.self (); request_id }));
  match await_result request_id `try_lock with
  | Ok acquired -> acquired
  | Error reason -> raise (Failure reason)
