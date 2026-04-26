open Kernel
open Collections

module Runtime_mutex = Kernel.Sync.Mutex
module Runtime_atomic = Kernel.Sync.Atomic

type t = {
  producer_lock: Runtime_mutex.t;
  mutable inbox_rev: Message.envelope list;
  mutable outbox: Message.envelope list;
  size: int Runtime_atomic.t;
}

let create = fun () ->
  {
    producer_lock = Runtime_mutex.create ();
    inbox_rev = [];
    outbox = [];
    size = Runtime_atomic.make 0;
  }

let queue = fun t msg ->
  Runtime_mutex.lock t.producer_lock;
  t.inbox_rev <- msg :: t.inbox_rev;
  let _ = Runtime_atomic.fetch_and_add t.size 1 in
  Runtime_mutex.unlock t.producer_lock

let pop_outbox = fun t ->
  match t.outbox with
  | [] -> None
  | msg :: rest ->
      t.outbox <- rest;
      let _ = Runtime_atomic.fetch_and_add t.size (-1) in
      Some msg

let next = fun t ->
  match pop_outbox t with
  | Some _ as msg -> msg
  | None ->
      Runtime_mutex.lock t.producer_lock;
      let drained = t.inbox_rev in
      t.inbox_rev <- [];
      Runtime_mutex.unlock t.producer_lock;
      if List.is_empty drained then
        None
      else (
        t.outbox <- List.reverse drained;
        pop_outbox t
      )

let size = fun t -> Runtime_atomic.get t.size

let is_empty = fun t -> Int.equal (Runtime_atomic.get t.size) 0
