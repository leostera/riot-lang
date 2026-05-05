open Kernel

module Runtime_atomic = Kernel.Sync.Atomic

type node = {
  msg: Message.envelope;
  next: node option Runtime_atomic.t;
}

type t = {
  inbox: node option Runtime_atomic.t;
  mutable outbox: node option;
  size: int Runtime_atomic.t;
}

let create = fun () -> {
  inbox = Runtime_atomic.make_contended None;
  outbox = None;
  size = Runtime_atomic.make_contended 0;
}

let queue = fun t msg ->
  let node = { msg; next = Runtime_atomic.make_contended None } in
  let rec push () =
    let head = Runtime_atomic.get t.inbox in
    Runtime_atomic.set node.next head;
    if Runtime_atomic.compare_and_set t.inbox head (Some node) then
      Runtime_atomic.incr t.size
    else
      push ()
  in
  push ()

let pop_outbox = fun t ->
  match t.outbox with
  | None -> None
  | Some node ->
      t.outbox <- Runtime_atomic.get node.next;
      Runtime_atomic.decr t.size;
      Some node.msg

let reverse_nodes = fun head ->
  let rec loop current previous =
    match current with
    | None -> previous
    | Some node ->
        let next = Runtime_atomic.get node.next in
        Runtime_atomic.set node.next previous;
        loop next current
  in
  loop head None

let next = fun t ->
  match pop_outbox t with
  | Some _ as msg -> msg
  | None ->
      let drained = Runtime_atomic.exchange t.inbox None in
      t.outbox <- reverse_nodes drained;
      pop_outbox t

let size = fun t -> Runtime_atomic.get t.size

let is_empty = fun t -> Int.equal (Runtime_atomic.get t.size) 0
