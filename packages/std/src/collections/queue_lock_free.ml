open Kernel

type 'value node = {
  value: 'value option;
  next: 'value node option Atomic.t;
}

type 'value t = {
  head: 'value node Atomic.t;
  tail: 'value node Atomic.t;
  size: int Atomic.t;
}

let make_node = fun value ->
  { value; next = Atomic.make_contended None }

let create = fun () ->
  let stub = make_node None in
  {
    head = Atomic.make_contended stub;
    tail = Atomic.make_contended stub;
    size = Atomic.make_contended 0;
  }

let with_capacity = fun ~size:_ -> create ()

let rec push_node = fun t new_node ->
  let tail = Atomic.get t.tail in
  let next = Atomic.get tail.next in
  if Ptr.equal tail (Atomic.get t.tail) then
    match next with
    | None ->
        if Atomic.compare_and_set tail.next None (Some new_node) then (
          let _ = Atomic.compare_and_set t.tail tail new_node in
          Atomic.incr t.size
        ) else
          push_node t new_node
    | Some next_node ->
        let _ = Atomic.compare_and_set t.tail tail next_node in
        push_node t new_node
  else
    push_node t new_node

let push = fun t ~value ->
  push_node t (make_node (Some value))

let from_list = fun values ->
  let queue = create () in
  List.for_each values ~fn:(fun value -> push queue ~value);
  queue

let rec pop = fun t ->
  let head = Atomic.get t.head in
  let tail = Atomic.get t.tail in
  let next = Atomic.get head.next in
  if Ptr.equal head (Atomic.get t.head) then
    match next with
    | None -> None
    | Some next_node ->
        if Ptr.equal head tail then (
          let _ = Atomic.compare_and_set t.tail tail next_node in
          pop t
        ) else
          match next_node.value with
          | None ->
              pop t
          | Some value ->
              if Atomic.compare_and_set t.head head next_node then (
                Atomic.decr t.size;
                Some value
              ) else
                pop t
  else
    pop t

let length = fun t -> Atomic.get t.size

let is_empty = fun t -> Int.equal (length t) 0

let clear = fun t ->
  let rec loop () =
    match pop t with
    | None -> ()
    | Some _ -> loop ()
  in
  loop ()
