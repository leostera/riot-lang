open Kernel

module Array = Kernel.Array

type 'a box = { mutable value: 'a }

let box = fun value -> { value }

type 'a t = {
  mutable data: 'a array;
  mutable size: int;
  compare: 'a -> 'a -> Order.t;
}

let create_with = fun ~compare () -> { data = [||]; size = 0; compare }

let create = fun () -> create_with ~compare ()

let create_max = fun () -> create_with ~compare:(fun a b -> compare b a) ()

let length = fun heap -> heap.size

let is_empty = fun heap -> heap.size = 0

let clear = fun heap -> heap.size <- 0

let ensure_capacity = fun heap ->
  let len = Array.length heap.data in
  if heap.size >= len then
    (
      let new_len =
        if len = 0 then
          8
        else
          len * 2
      in
      let new_data = Array.make ~count:new_len ~value:(Array.get_unchecked heap.data ~at:0) in
      Array.blit heap.data ~src_offset:0 ~dst:new_data ~dst_offset:0 ~len:heap.size;
      heap.data <- new_data
    )

let parent = fun i -> (i - 1) / 2

let left = fun i -> (2 * i) + 1

let right = fun i -> (2 * i) + 2

let swap = fun heap i j ->
  let temp = Array.get_unchecked heap.data ~at:i in
  Array.set_unchecked
    heap.data
    ~at:i
    ~value:(Array.get_unchecked heap.data ~at:j);
  Array.set_unchecked heap.data ~at:j ~value:temp

let rec sift_up = fun heap i ->
  if i > 0 then
    let p = parent i in
    match heap.compare (Array.get_unchecked heap.data ~at:i) (Array.get_unchecked heap.data ~at:p) with
    | Order.LT ->
        swap heap i p;
        sift_up heap p
    | Order.EQ
    | Order.GT -> ()

let rec sift_down = fun heap i ->
  let l = left i in
  let r = right i in
  let smallest = box i in
  if l < heap.size then
    (
      match heap.compare
        (Array.get_unchecked heap.data ~at:l)
        (Array.get_unchecked heap.data ~at:smallest.value) with
      | Order.LT -> smallest.value <- l
      | Order.EQ
      | Order.GT -> ()
    );
  if r < heap.size then
    (
      match heap.compare
        (Array.get_unchecked heap.data ~at:r)
        (Array.get_unchecked heap.data ~at:smallest.value) with
      | Order.LT -> smallest.value <- r
      | Order.EQ
      | Order.GT -> ()
    );
  let smallest_val = smallest.value in
  if smallest_val != i then
    (
      swap heap i smallest_val;
      sift_down heap smallest_val
    )

let push = fun heap ~value ->
  if heap.size = 0 then
    heap.data <- Array.make ~count:8 ~value
  else
    ensure_capacity heap;
  Array.set_unchecked heap.data ~at:heap.size ~value;
  sift_up heap heap.size;
  heap.size <- heap.size + 1

let peek = fun heap ->
  if heap.size = 0 then
    None
  else
    Some (Array.get_unchecked heap.data ~at:0)

let peek_unchecked = fun heap ->
  if heap.size = 0 then
    Kernel.SystemError.panic "Heap.peek_unchecked called on an empty heap"
  else
    Array.get_unchecked heap.data ~at:0

let pop = fun heap ->
  if heap.size = 0 then
    None
  else
    let result = Array.get_unchecked heap.data ~at:0 in
    heap.size <- heap.size - 1;
  if heap.size > 0 then
    (
      Array.set_unchecked
        heap.data
        ~at:0
        ~value:(Array.get_unchecked heap.data ~at:heap.size);
      sift_down heap 0
    );
  Some result

let pop_unchecked = fun heap ->
  match pop heap with
  | Some v -> v
  | None -> Kernel.SystemError.panic "Heap.pop_unchecked called on an empty heap"

let heapify = fun heap ->
  for i = (heap.size / 2) - 1 downto 0 do
    sift_down heap i
  done

let from_list_with = fun ~compare list ->
  match list with
  | [] -> create_with ~compare ()
  | first :: rest ->
      let len = List.length list in
      let data = Array.make ~count:len ~value:first in
      let rec fill index = function
        | [] -> ()
        | value :: rest ->
            Array.set_unchecked data ~at:index ~value;
            fill (index + 1) rest
      in
      fill 0 list;
      let heap = { data; size = len; compare } in
      heapify heap;
      heap

let from_list = fun list -> from_list_with ~compare list

let to_list = fun heap ->
  let result = box [] in
  while heap.size > 0 do
    match pop heap with
    | Some x -> result.value <- x :: result.value
    | None -> ()
  done;
  List.reverse result.value

let to_list_unordered = fun heap ->
  let result = box [] in
  for i = heap.size - 1 downto 0 do
    result.value <- Array.get_unchecked heap.data ~at:i :: result.value
  done;
  result.value

let for_each = fun heap ~fn ->
  while heap.size > 0 do
    match pop heap with
    | Some x -> fn x
    | None -> ()
  done

let fold_left = fun heap ~init ~fn ->
  let result = box init in
  while heap.size > 0 do
    match pop heap with
    | Some x -> result.value <- fn result.value x
    | None -> ()
  done;
  result.value

let iter: type item. item t -> item Iter.Iterator.t = fun heap ->
  let module HeapIter = struct
    type state = item t

    type nonrec item = item

    let next = fun state ->
      match pop state with
      | None -> (None, state)
      | Some value -> (Some value, state)

    let size = fun state -> state.size
  end in
  let heap_copy = { data = Array.clone heap.data; size = heap.size; compare = heap.compare } in
  Iter.Iterator.make (module HeapIter) heap_copy

let mut_iter: type item. item t -> item Iter.MutIterator.t = fun heap ->
  let module HeapIter = struct
    type state = item t

    type nonrec item = item

    let next = fun state -> pop state

    let size = fun state -> state.size

    let clone = fun state -> {
      data = Array.clone state.data;
      size = state.size;
      compare = state.compare;
    }
  end in
  Iter.MutIterator.make (module HeapIter) heap
