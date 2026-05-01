open Kernel.Prelude

module Runtime_pid = Runtime.Pid
module Waiters = Kernel.Queue

type t = {
  pid: Runtime_pid.t;
}

type request_id = int

type waiter = {
  pid: Runtime_pid.t;
  request_id: request_id;
}

type request =
  | Wait of {
      reply_to: Runtime_pid.t;
      request_id: request_id;
      mutex: Mutex.t;
    }
  | Signal
  | Broadcast

type Runtime.Message.t +=
  | Sync_condition_request of request
  | Sync_condition_signaled of {
      request_id: request_id;
    }
  | Sync_condition_failed of {
      request_id: request_id;
      reason: string;
    }

let request_ids = Atomic.make 0

let next_request_id = fun () -> Int.succ (Atomic.fetch_and_add request_ids 1)

let signal_waiter = fun waiter ->
  Runtime.send
    waiter.pid
    (Sync_condition_signaled { request_id = waiter.request_id })

let fail_waiter = fun waiter reason ->
  Runtime.send
    waiter.pid
    (Sync_condition_failed { request_id = waiter.request_id; reason })

let await_mutex_suspend = fun mutex ~owner -> Mutex.suspend mutex ~owner

let rec signal_all = fun waiters ->
  match Waiters.pop waiters with
  | None -> ()
  | Some waiter ->
      signal_waiter waiter;
      signal_all waiters

let rec loop = fun waiters ->
  let selector msg =
    match msg with
    | Sync_condition_request request -> Runtime.Select request
    | _ -> Runtime.Skip
  in
  match Runtime.receive ~selector () with
  | Wait { reply_to; request_id; mutex } ->
      let waiter = { pid = reply_to; request_id } in
      (
        match await_mutex_suspend mutex ~owner:reply_to with
        | Ok () ->
            Waiters.push waiters ~value:waiter;
            loop waiters
        | Error reason ->
            fail_waiter waiter reason;
            loop waiters
      )
  | Signal ->
      (
        match Waiters.pop waiters with
        | None -> ()
        | Some waiter -> signal_waiter waiter
      );
      loop waiters
  | Broadcast ->
      signal_all waiters;
      loop waiters

let create = fun () ->
  {
    pid = Runtime.spawn (fun () -> loop (Waiters.create ()));
  }

let wait = fun (t: t) (mutex: Mutex.t) ->
  let request_id = next_request_id () in
  Runtime.send
    t.pid
    (Sync_condition_request (Wait { reply_to = Runtime.self (); request_id; mutex }));
  let selector msg =
    match msg with
    | Sync_condition_signaled { request_id = got } when Int.equal got request_id ->
        Runtime.Select (Ok ())
    | Sync_condition_failed { request_id = got; reason } when Int.equal got request_id ->
        Runtime.Select (Error reason)
    | _ -> Runtime.Skip
  in
  match Runtime.receive ~selector () with
  | Ok () -> Mutex.lock mutex
  | Error reason -> raise (Failure reason)

let signal = fun (t: t) -> Runtime.send t.pid (Sync_condition_request Signal)

let broadcast = fun (t: t) -> Runtime.send t.pid (Sync_condition_request Broadcast)
