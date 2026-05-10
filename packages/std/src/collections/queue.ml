module StdList = List

open Kernel

(* Michael-Scott style linked FIFO queue with a consumed dummy head node.
   Enqueue and dequeue stay on CAS loops; snapshot helpers walk the currently
   reachable linked list without trying to freeze concurrent mutation.
*)

type 'value node = {
  mutable value: 'value option;
  next: 'value node option Atomic.t;
}

type 'value t = {
  head: 'value node Atomic.t;
  tail: 'value node Atomic.t;
  size: int Atomic.t;
}

let make_node = fun value -> { value; next = Atomic.make_contended None }

let create = fun () ->
  let stub = make_node None in
  {
    head = Atomic.make_contended stub;
    tail = Atomic.make_contended stub;
    size = Atomic.make_contended 0;
  }

let with_capacity = fun ~size:_ -> create ()

let push = fun t ~value ->
  let new_node = make_node (Some value) in
  let rec loop () =
    let tail = Atomic.get t.tail in
    let next = Atomic.get tail.next in
    if Ptr.equal tail (Atomic.get t.tail) then
      match next with
      | None ->
          if Atomic.compare_and_set tail.next None (Some new_node) then (
            let _ = Atomic.compare_and_set t.tail tail new_node in
            Atomic.incr t.size
          ) else
            loop ()
      | Some next_node ->
          let _ = Atomic.compare_and_set t.tail tail next_node in
          loop ()
    else
      loop ()
  in
  loop ()

let pop = fun t ->
  let rec loop () =
    let head = Atomic.get t.head in
    let tail = Atomic.get t.tail in
    let next = Atomic.get head.next in
    if Ptr.equal head (Atomic.get t.head) then
      match next with
      | None -> None
      | Some next_node ->
          if Ptr.equal head tail then (
            let _ = Atomic.compare_and_set t.tail tail next_node in
            loop ()
          ) else if Atomic.compare_and_set t.head head next_node then (
            Atomic.decr t.size;
            match next_node.value with
            | None -> loop ()
            | Some value ->
                next_node.value <- None;
                Some value
          ) else
            loop ()
    else
      loop ()
  in
  loop ()

let from_list = fun values ->
  let queue = create () in
  StdList.for_each values ~fn:(fun value -> push queue ~value);
  queue

let front = fun t ->
  let rec loop () =
    let head = Atomic.get t.head in
    match Atomic.get head.next with
    | None -> None
    | Some next_node ->
        match next_node.value with
        | Some value -> Some value
        | None -> loop ()
  in
  loop ()

let length = fun t -> Atomic.get t.size

let is_empty = fun t -> Option.is_none (front t)

let clear = fun t ->
  let rec loop () =
    match pop t with
    | None -> ()
    | Some _ -> loop ()
  in
  loop ()

(* The head always points at a consumed stub node, so snapshot reads start at
   [head.next] and skip any nodes that concurrent consumers already cleared.
*)

let snapshot_values = fun t ->
  let rec loop node_opt acc =
    match node_opt with
    | None -> StdList.reverse acc
    | Some node ->
        let acc =
          match node.value with
          | None -> acc
          | Some value -> value :: acc
        in
        loop (Atomic.get node.next) acc
  in
  let head = Atomic.get t.head in
  loop (Atomic.get head.next) []

let for_each = fun t ~fn -> StdList.for_each (snapshot_values t) ~fn

let fold_left = fun t ~init ~fn -> StdList.fold_left (snapshot_values t) ~init ~fn

let to_list = snapshot_values

let contains = fun t ~value -> StdList.any (snapshot_values t) ~fn:(fun item -> item = value)

let transfer = fun ~src ~dst ->
  let rec loop () =
    match pop src with
    | None -> ()
    | Some value ->
        push dst ~value;
        loop ()
  in
  loop ()

let append = fun left right -> transfer ~src:right ~dst:left

let iter: type item. item t -> item Iter.Iterator.t = fun queue ->
  let module QueueIter = struct
    type state = item list

    type nonrec item = item

    let next = fun __tmp1 ->
      match __tmp1 with
      | [] -> (None, [])
      | value :: rest -> (Some value, rest)

    let size = StdList.length
  end in
  Iter.Iterator.make (module QueueIter) (snapshot_values queue)

let mut_iter: type item. item t -> item Iter.MutIterator.t = fun queue ->
  let module QueueIter = struct
    type state = item t

    type nonrec item = item

    let next = pop

    let size = length

    let clone = fun queue -> from_list (snapshot_values queue)
  end in
  Iter.MutIterator.make (module QueueIter) queue
