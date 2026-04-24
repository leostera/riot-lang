open Kernel

module Runtime_mutex = Kernel.Sync.Mutex
module Runtime_atomic = Kernel.Sync.Atomic

type 'value t = {
  lock: Runtime_mutex.t;
  queue: 'value Queue_core.t;
  size: int Runtime_atomic.t;
}

let create = fun () ->
  { lock = Runtime_mutex.create (); queue = Queue_core.create (); size = Runtime_atomic.make 0 }

let with_capacity = fun ~size:_ -> create ()

let with_lock = fun t fn ->
  Runtime_mutex.lock t.lock;
  try
    let result = fn () in
    Runtime_mutex.unlock t.lock;
    result
  with
  | exn ->
      Runtime_mutex.unlock t.lock;
      raise exn

let push = fun t ~value ->
  with_lock t (fun () ->
    Queue_core.push t.queue ~value;
    let _ = Runtime_atomic.fetch_and_add t.size 1 in
    ())

let from_list = fun values ->
  let queue = create () in
  List.for_each values ~fn:(fun value -> push queue ~value);
  queue

let pop = fun t ->
  with_lock t (fun () ->
    match Queue_core.pop t.queue with
    | None -> None
    | Some _ as value ->
        let _ = Runtime_atomic.fetch_and_add t.size (-1) in
        value)

let length = fun t -> Runtime_atomic.get t.size

let is_empty = fun t -> Int.equal (length t) 0

let clear = fun t ->
  with_lock t (fun () ->
    let removed = Queue_core.length t.queue in
    Queue_core.clear t.queue;
    if removed > 0 then
      let _ = Runtime_atomic.fetch_and_add t.size (-removed) in
      ())
