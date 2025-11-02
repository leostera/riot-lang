open Global

type 'a t = {
  mutable data : 'a array;
  mutable size : int;
  compare : 'a -> 'a -> int;
}

let create_with ~compare () = { data = [||]; size = 0; compare }
let create () = create_with ~compare ()
let create_max () = create_with ~compare:(fun a b -> compare b a) ()
let size heap = heap.size
let is_empty heap = heap.size = 0
let clear heap = heap.size <- 0

let ensure_capacity heap =
  let len = Array.length heap.data in
  if heap.size >= len then (
    let new_len = if len = 0 then 8 else len * 2 in
    let new_data = Array.make new_len heap.data.(0) in
    Array.blit heap.data 0 new_data 0 heap.size;
    heap.data <- new_data)

let parent i = (i - 1) / 2
let left i = (2 * i) + 1
let right i = (2 * i) + 2

let swap heap i j =
  let temp = heap.data.(i) in
  heap.data.(i) <- heap.data.(j);
  heap.data.(j) <- temp

let rec sift_up heap i =
  if i > 0 then
    let p = parent i in
    if heap.compare heap.data.(i) heap.data.(p) < 0 then (
      swap heap i p;
      sift_up heap p)

let rec sift_down heap i =
  let l = left i in
  let r = right i in
  let smallest = cell i in

  if
    l < heap.size
    && heap.compare heap.data.(l) heap.data.(Cell.get smallest) < 0
  then Cell.set smallest l;

  if
    r < heap.size
    && heap.compare heap.data.(r) heap.data.(Cell.get smallest) < 0
  then Cell.set smallest r;

  if Cell.get smallest <> i then (
    swap heap i (Cell.get smallest);
    sift_down heap (Cell.get smallest))

let push heap value =
  if heap.size = 0 then heap.data <- Array.make 8 value
  else ensure_capacity heap;

  heap.data.(heap.size) <- value;
  sift_up heap heap.size;
  heap.size <- heap.size + 1

let peek heap = if heap.size = 0 then None else Some heap.data.(0)
let peek_exn heap = if heap.size = 0 then raise Not_found else heap.data.(0)

let pop heap =
  if heap.size = 0 then None
  else
    let result = heap.data.(0) in
    heap.size <- heap.size - 1;
    if heap.size > 0 then (
      heap.data.(0) <- heap.data.(heap.size);
      sift_down heap 0);
    Some result

let pop_exn heap = match pop heap with Some v -> v | None -> raise Not_found

let heapify heap =
  for i = (heap.size / 2) - 1 downto 0 do
    sift_down heap i
  done

let of_list_with ~compare list =
  match list with
  | [] -> create_with ~compare ()
  | first :: rest ->
      let len = List.length list in
      let data = Array.make len first in
      List.iteri (fun i x -> data.(i) <- x) list;
      let heap = { data; size = len; compare } in
      heapify heap;
      heap

let of_list list = of_list_with ~compare list

let to_list heap =
  let result = cell [] in
  while heap.size > 0 do
    match pop heap with
    | Some x -> Cell.set result (x :: Cell.get result)
    | None -> ()
  done;
  List.rev (Cell.get result)

let to_list_unordered heap =
  let result = cell [] in
  for i = heap.size - 1 downto 0 do
    Cell.set result (heap.data.(i) :: Cell.get result)
  done;
  Cell.get result

let iter f heap =
  while heap.size > 0 do
    match pop heap with Some x -> f x | None -> ()
  done

let fold f acc heap =
  let result = cell acc in
  while heap.size > 0 do
    match pop heap with
    | Some x -> Cell.set result (f (Cell.get result) x)
    | None -> ()
  done;
  Cell.get result

let into_iter : type item. item t -> item Iter.Iterator.t =
 fun heap ->
  let module HeapIter = struct
    type state = item t
    type nonrec item = item

    let next state =
      match pop state with
      | None -> (None, state)
      | Some value -> (Some value, state)

    let size state = state.size
  end in
  let heap_copy = { data = Array.copy heap.data; size = heap.size; compare = heap.compare } in
  Iter.Iterator.make (module HeapIter) heap_copy

let to_mut_iter : type item. item t -> item Iter.MutIterator.t =
 fun heap ->
  let module HeapIter = struct
    type state = item t
    type nonrec item = item

    let next state = pop state
    let size state = state.size

    let clone state =
      {
        data = Array.copy state.data;
        size = state.size;
        compare = state.compare;
      }
  end in
  Iter.MutIterator.make (module HeapIter) heap
