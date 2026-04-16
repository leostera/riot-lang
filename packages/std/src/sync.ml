open Kernel
module Runtime_atomic = Atomic
module Runtime_mutex = Mutex
module Runtime_condition = Condition
module Runtime_actor = Runtime.Actor
module Runtime_pid = Runtime.Pid

module Atomic = struct
  include Runtime_atomic
end

module Cell = struct
  type 'a t = {
    mutable value: 'a;
  }

  let create = fun value -> { value }

  let get = fun cell -> cell.value

  let ( ! ) = get

  let set = fun cell value -> cell.value <- value

  let ( := ) = set

  let update = fun cell f -> cell.value <- f cell.value

  let incr = fun cell -> cell.value <- cell.value + 1

  let decr = fun cell -> cell.value <- cell.value - 1

  let replace = fun cell new_value ->
    let old_value = cell.value in
    cell.value <- new_value;
    old_value

  let take = fun cell ~default ->
    let old_value = cell.value in
    cell.value <- default;
    old_value

  let swap = fun left right ->
    let temp = left.value in
    left.value <- right.value;
    right.value <- temp

  let compare_and_swap = fun cell expected new_value ->
    if cell.value = expected then
      (
        cell.value <- new_value;
        true
      )
    else
      false

  let equal = fun left right -> left.value = right.value
end

module Mutex = struct
  module Waiters = Collections.Queue

  type t = { pid: Runtime_pid.t }

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
    | Sync_mutex_try_result of {
        request_id: request_id;
        acquired: bool;
      }
    | Sync_mutex_released of { request_id: request_id }
    | Sync_mutex_suspended of { request_id: request_id }
    | Sync_mutex_failed of {
        request_id: request_id;
        reason: string;
      }

  type state = {
    mutable owner: owner option;
    waiters: (Runtime_pid.t * request_id) Waiters.t;
  }

  let request_ids = Runtime_atomic.make 0

  let next_request_id = fun () ->
    Runtime_atomic.fetch_and_add request_ids 1 + 1

  let fail = fun reply_to request_id reason ->
    Runtime.send reply_to (Sync_mutex_failed { request_id; reason })

  let grant = fun state pid request_id ->
    let monitor = Runtime_actor.monitor pid in
    state.owner <- Some { pid; monitor };
    Runtime.send pid (Sync_mutex_acquired { request_id })

  let rec grant_next = fun state ->
    match Waiters.pop state.waiters with
    | None ->
        state.owner <- None
    | Some (pid, request_id) ->
        grant state pid request_id

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
    | Some _ ->
        fail reply_to request_id "mutex unlock by non-owner"
    | None ->
        fail reply_to request_id "mutex unlock while unlocked"

  let handle_suspend = fun state owner_pid reply_to request_id ->
    match state.owner with
    | Some owner when Runtime_pid.equal owner.pid owner_pid ->
        release_owner state;
        Runtime.send reply_to (Sync_mutex_suspended { request_id });
        grant_next state
    | Some _ ->
        fail reply_to request_id "mutex wait without ownership"
    | None ->
        fail reply_to request_id "mutex wait while unlocked"

  let release_owner_on_exit = fun state monitor_ref pid ->
    match state.owner with
    | Some owner when owner.monitor = monitor_ref && Runtime_pid.equal owner.pid pid ->
        (* already down; no demonitor needed *)
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
          | Some _ ->
              Runtime.send reply_to (Sync_mutex_try_result { request_id; acquired = false })
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
    { pid = Runtime.spawn (fun () -> loop { owner = None; waiters = Waiters.create () }) }

  let await_result = fun request_id expected ->
    let selector msg =
      match msg with
      | Sync_mutex_acquired { request_id = got }
        when expected = `acquired && Int.equal got request_id -> `select (Ok true)
      | Sync_mutex_released { request_id = got }
        when expected = `released && Int.equal got request_id -> `select (Ok true)
      | Sync_mutex_try_result { request_id = got; acquired }
        when expected = `try_lock && Int.equal got request_id -> `select (Ok acquired)
      | Sync_mutex_failed { request_id = got; reason }
        when Int.equal got request_id -> `select (Error reason)
      | _ -> `skip
    in
    Runtime.receive ~selector ()

  let lock = fun (t : t) ->
    let request_id = next_request_id () in
    Runtime.send t.pid (Sync_mutex_request (Acquire { reply_to = Runtime.self (); request_id }));
    match await_result request_id `acquired with
    | Ok _ -> ()
    | Error reason -> raise (Failure reason)

  let unlock = fun (t : t) ->
    let request_id = next_request_id () in
    Runtime.send t.pid (Sync_mutex_request (Release { reply_to = Runtime.self (); request_id }));
    match await_result request_id `released with
    | Ok _ -> ()
    | Error reason -> raise (Failure reason)

  let try_lock = fun (t : t) ->
    let request_id = next_request_id () in
    Runtime.send t.pid (Sync_mutex_request (Try_acquire { reply_to = Runtime.self (); request_id }));
    match await_result request_id `try_lock with
    | Ok acquired -> acquired
    | Error reason -> raise (Failure reason)
end

module Condition = struct
  module Waiters = Collections.Queue

  type t = { pid: Runtime_pid.t }

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
    | Sync_condition_signaled of { request_id: request_id }
    | Sync_condition_failed of {
        request_id: request_id;
        reason: string;
      }

  let request_ids = Runtime_atomic.make 0

  let next_request_id = fun () ->
    Runtime_atomic.fetch_and_add request_ids 1 + 1

  let signal_waiter = fun waiter ->
    Runtime.send waiter.pid (Sync_condition_signaled { request_id = waiter.request_id })

  let fail_waiter = fun waiter reason ->
    Runtime.send waiter.pid (Sync_condition_failed { request_id = waiter.request_id; reason })

  let await_mutex_suspend = fun request_id ->
    let selector msg =
      match msg with
      | Mutex.Sync_mutex_suspended { request_id = got }
        when Int.equal got request_id -> `select (Ok ())
      | Mutex.Sync_mutex_failed { request_id = got; reason }
        when Int.equal got request_id -> `select (Error reason)
      | _ -> `skip
    in
    Runtime.receive ~selector ()

  let rec signal_all = fun waiters ->
    match Waiters.pop waiters with
    | None -> ()
    | Some waiter ->
        signal_waiter waiter;
        signal_all waiters

  let rec loop = fun waiters ->
    let selector msg =
      match msg with
      | Sync_condition_request request -> `select request
      | _ -> `skip
    in
    match Runtime.receive ~selector () with
    | Wait { reply_to; request_id; mutex } ->
        let waiter = { pid = reply_to; request_id } in
        let suspend_id = Mutex.next_request_id () in
        Runtime.send mutex.pid
          (Mutex.Sync_mutex_request
             (Mutex.Suspend {
                owner = reply_to;
                reply_to = Runtime.self ();
                request_id = suspend_id;
              }));
        (
          match await_mutex_suspend suspend_id with
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
    { pid = Runtime.spawn (fun () -> loop (Waiters.create ())) }

  let wait = fun (t : t) (mutex : Mutex.t) ->
    let request_id = next_request_id () in
    Runtime.send t.pid (Sync_condition_request (Wait { reply_to = Runtime.self (); request_id; mutex }));
    let selector msg =
      match msg with
      | Sync_condition_signaled { request_id = got }
        when Int.equal got request_id -> `select (Ok ())
      | Sync_condition_failed { request_id = got; reason }
        when Int.equal got request_id -> `select (Error reason)
      | _ -> `skip
    in
    match Runtime.receive ~selector () with
    | Ok () -> Mutex.lock mutex
    | Error reason -> raise (Failure reason)

  let signal = fun (t : t) ->
    Runtime.send t.pid (Sync_condition_request Signal)

  let broadcast = fun (t : t) ->
    Runtime.send t.pid (Sync_condition_request Broadcast)
end

module OnceCell = struct
  type 'a t = 'a option Cell.t

  let create = fun () -> Cell.create None

  let get = fun cell -> Cell.get cell

  let take = fun cell ->
    let value = Cell.get cell in
    Cell.set cell None;
    value

  let get_or_init = fun cell f ->
    match Cell.get cell with
    | Some value -> value
    | None ->
        let value = f () in
        Cell.set cell (Some value);
        value

  let get_or_try_init = fun cell f ->
    match Cell.get cell with
    | Some value -> Ok value
    | None -> (
        match f () with
        | Ok value ->
            Cell.set cell (Some value);
            Ok value
        | Error _ as error -> error
      )

  let set = fun cell value ->
    match Cell.get cell with
    | None ->
        Cell.set cell (Some value);
        Ok ()
    | Some _ -> Error `AlreadyInitialized

  let is_initialized = fun cell ->
    match Cell.get cell with
    | Some _ -> true
    | None -> false
end

module LazyCell = struct
  type 'a t = {
    storage: 'a option Cell.t;
    init: unit -> 'a;
  }

  let create = fun init -> { storage = Cell.create None; init }

  let force = fun lazy_cell ->
    match Cell.get lazy_cell.storage with
    | Some value -> value
    | None ->
        let value = lazy_cell.init () in
        Cell.set lazy_cell.storage (Some value);
        value

  let is_initialized = fun lazy_cell ->
    match Cell.get lazy_cell.storage with
    | Some _ -> true
    | None -> false

  let take = fun lazy_cell ->
    let value = Cell.get lazy_cell.storage in
    Cell.set lazy_cell.storage None;
    value

  let get = force
end

module RefCell = struct
  type borrow_state =
    | Available
    | Borrowed of int
    | BorrowedMut

  type 'a t = {
    mutable value: 'a;
    mutable state: borrow_state;
  }

  exception BorrowError of string

  exception BorrowMutError of string

  let create = fun value -> { value; state = Available }

  type 'a borrow = 'a t * 'a

  let borrow = fun cell ->
    match cell.state with
    | Available ->
        cell.state <- Borrowed 1;
        (cell, cell.value)
    | Borrowed count ->
        cell.state <- Borrowed (count + 1);
        (cell, cell.value)
    | BorrowedMut ->
        raise (BorrowError "Cannot borrow while mutably borrowed")

  let release_borrow = fun (cell, _) ->
    match cell.state with
    | Borrowed 1 -> cell.state <- Available
    | Borrowed count -> cell.state <- Borrowed (count - 1)
    | _ -> ()

  type 'a borrow_mut = 'a t

  let borrow_mut = fun cell ->
    match cell.state with
    | Available ->
        cell.state <- BorrowedMut;
        cell
    | Borrowed _ ->
        raise (BorrowMutError "Cannot mutably borrow while borrowed")
    | BorrowedMut ->
        raise (BorrowMutError "Already mutably borrowed")

  let get_mut = fun cell ->
    match cell.state with
    | BorrowedMut -> cell.value
    | _ -> raise (BorrowMutError "Not mutably borrowed")

  let set_mut = fun cell value ->
    match cell.state with
    | BorrowedMut -> cell.value <- value
    | _ -> raise (BorrowMutError "Not mutably borrowed")

  let release_borrow_mut = fun cell ->
    match cell.state with
    | BorrowedMut -> cell.state <- Available
    | _ -> ()

  let with_borrow = fun cell f ->
    let borrow = borrow cell in
    let _, value = borrow in
    let result = f value in
    release_borrow borrow;
    result

  let with_borrow_mut = fun cell f ->
    let borrow = borrow_mut cell in
    let result =
      f (fun () -> get_mut borrow) (fun value -> set_mut borrow value)
    in
    release_borrow_mut borrow;
    result

  let try_borrow = fun cell ->
    try Ok (borrow cell) with
    | BorrowError message -> Error message

  let try_borrow_mut = fun cell ->
    try Ok (borrow_mut cell) with
    | BorrowMutError message -> Error message

  let get_unchecked = fun cell -> cell.value

  let set_unchecked = fun cell value -> cell.value <- value

  let is_borrowed = fun cell ->
    match cell.state with
    | Available -> false
    | _ -> true

  let borrow_count = fun cell ->
    match cell.state with
    | Available -> 0
    | Borrowed count -> count
    | BorrowedMut -> 1
end
