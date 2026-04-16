open Kernel
open Collections
open Sync
module Runtime_mutex = Kernel.Sync.Mutex

type t = {
  producer_lock: Runtime_mutex.t;
  mutable inbox_rev: Message.envelope list;
  mutable outbox: Message.envelope list;
  size: int Atomic.t;
}

let create = fun () ->
  { producer_lock = Runtime_mutex.create (); inbox_rev = []; outbox = []; size = Atomic.make 0 }

let queue = fun t msg ->
  Runtime_mutex.lock t.producer_lock;
  t.inbox_rev <- msg :: t.inbox_rev;
  let _ = Atomic.fetch_and_add t.size 1 in
  Runtime_mutex.unlock t.producer_lock

let pop_outbox = fun t ->
  match t.outbox with
  | [] -> None
  | msg :: rest ->
      t.outbox <- rest;
      let _ = Atomic.fetch_and_add t.size (-1) in
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

let size = fun t -> Atomic.get t.size

let is_empty = fun t ->
  Int.equal (Atomic.get t.size) 0
