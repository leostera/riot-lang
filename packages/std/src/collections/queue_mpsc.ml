open Kernel

module Runtime_mutex = Kernel.Sync.Mutex
module Runtime_atomic = Kernel.Sync.Atomic

type 'value t = {
  producer_lock: Runtime_mutex.t;
  mutable inbox_rev: 'value list;
  mutable outbox: 'value list;
  size: int Runtime_atomic.t;
}

let create = fun () ->
  {
    producer_lock = Runtime_mutex.create ();
    inbox_rev = [];
    outbox = [];
    size = Runtime_atomic.make 0
  }

let with_capacity = fun ~size:_ -> create ()

let with_producer_lock = fun t fn ->
  Runtime_mutex.lock t.producer_lock;
  try
    let result = fn () in
    Runtime_mutex.unlock t.producer_lock;
    result
  with
  | exn ->
      Runtime_mutex.unlock t.producer_lock;
      raise exn

let push = fun t ~value ->
  with_producer_lock t (fun () ->
    t.inbox_rev <- value :: t.inbox_rev;
    let _ = Runtime_atomic.fetch_and_add t.size 1 in
    ())

let from_list = fun values ->
  let queue = create () in
  List.for_each values ~fn:(fun value -> push queue ~value);
  queue

let pop_outbox = fun t ->
  match t.outbox with
  | [] -> None
  | value :: rest ->
      t.outbox <- rest;
      let _ = Runtime_atomic.fetch_and_add t.size (-1) in
      Some value

let pop = fun t ->
  match pop_outbox t with
  | Some _ as value -> value
  | None ->
      let drained =
        with_producer_lock t (fun () ->
          let drained = t.inbox_rev in
          t.inbox_rev <- [];
          drained)
      in
      if List.is_empty drained then
        None
      else (
        t.outbox <- List.reverse drained;
        pop_outbox t
      )

let length = fun t -> Runtime_atomic.get t.size

let is_empty = fun t -> Int.equal (length t) 0

let clear = fun t ->
  with_producer_lock t (fun () ->
    let removed = List.length t.inbox_rev + List.length t.outbox in
    t.inbox_rev <- [];
    t.outbox <- [];
    if removed > 0 then
      let _ = Runtime_atomic.fetch_and_add t.size (-removed) in
      ())
