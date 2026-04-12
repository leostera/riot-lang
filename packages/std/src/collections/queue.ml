open Kernel

type 'a node = {
  value: 'a;
  mutable next: 'a node option;
}

type 'a t = {
  mutable front: 'a node option;
  mutable back: 'a node option;
  mutable length: int;
}

let create = fun () -> { front = None; back = None; length = 0 }

let with_capacity = fun ~size:_ -> create ()

let length = fun queue -> queue.length

let is_empty = fun queue -> queue.length = 0

let push = fun queue ~value ->
  let new_node = { value; next = None } in
  match queue.back with
  | None ->
      queue.front <- Some new_node;
      queue.back <- Some new_node;
      queue.length <- queue.length + 1
  | Some back_node ->
      back_node.next <- Some new_node;
      queue.back <- Some new_node;
      queue.length <- queue.length + 1

let pop = fun queue ->
  match queue.front with
  | None -> None
  | Some front_node ->
      queue.front <- front_node.next;
      if queue.front = None then
        queue.back <- None;
      queue.length <- queue.length - 1;
      Some front_node.value

let front = fun queue ->
  match queue.front with
  | None -> None
  | Some front_node -> Some front_node.value

let clear = fun queue ->
  queue.front <- None;
  queue.back <- None;
  queue.length <- 0

let for_each = fun queue ~fn ->
  let rec loop node =
    match node with
    | None -> ()
    | Some n ->
        fn n.value;
        loop n.next
  in
  loop queue.front

let fold_left = fun queue ~acc ~fn ->
  let rec loop node acc =
    match node with
    | None -> acc
    | Some n -> loop n.next (fn acc n.value)
  in
  loop queue.front acc

let to_list = fun queue ->
  let rec loop node acc =
    match node with
    | None -> List.reverse acc
    | Some n -> loop n.next (n.value :: acc)
  in
  loop queue.front []

let contains = fun queue ~value ->
  let rec loop node =
    match node with
    | None -> false
    | Some n -> n.value = value || loop n.next
  in
  loop queue.front

let append = fun queue1 queue2 ->
  for_each queue2 ~fn:(fun value -> push queue1 ~value);
  clear queue2

let transfer = fun ~src ~dst ->
  match src.front with
  | None -> ()
  | Some _ ->
      (
        match dst.back with
        | None ->
            (* dst is empty, just move everything *)
            dst.front <- src.front;
            dst.back <- src.back;
            dst.length <- src.length
        | Some back_node ->
            (* dst has items, append src to the end *)
            back_node.next <- src.front;
            dst.back <- src.back;
            dst.length <- dst.length + src.length
      );
      (* Clear src queue *)
      src.front <- None;
      src.back <- None;
      src.length <- 0

let from_list = fun elements ->
  let queue = create () in
  List.for_each elements ~fn:(fun value -> push queue ~value);
  queue

let iter: type item. item t -> item Iter.Iterator.t = fun queue ->
  let module QueueIter = struct
    type state = item node option

    type nonrec item = item

    let next = fun state ->
      match state with
      | None -> (None, None)
      | Some node -> (Some node.value, node.next)

    let size = fun state ->
      let rec count = function
        | None -> 0
        | Some n -> 1 + count n.next
      in
      count state
  end in
  Iter.Iterator.make (module QueueIter) queue.front

let mut_iter: type item. item t -> item Iter.MutIterator.t = fun queue ->
  let module QueueIter = struct
    type state = item t

    type nonrec item = item

    let next = fun queue -> pop queue

    let size = fun queue -> length queue

    let clone = fun queue ->
      let queue2 = with_capacity ~size:(length queue) in
      for_each queue ~fn:(fun value -> push queue2 ~value);
      queue2
  end in
  Iter.MutIterator.make (module QueueIter) queue
