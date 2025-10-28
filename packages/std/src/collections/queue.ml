type 'a node = { value : 'a; mutable next : 'a node option }

type 'a t = {
  mutable front : 'a node option;
  mutable back : 'a node option;
  mutable length : int;
}

let create () = { front = None; back = None; length = 0 }
let with_capacity _ = create ()
let len queue = queue.length
let is_empty queue = queue.length = 0

let enqueue queue value =
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

let dequeue queue =
  match queue.front with
  | None -> None
  | Some front_node ->
      queue.front <- front_node.next;
      if queue.front = None then queue.back <- None;
      queue.length <- queue.length - 1;
      Some front_node.value

let front queue =
  match queue.front with
  | None -> None
  | Some front_node -> Some front_node.value

let clear queue =
  queue.front <- None;
  queue.back <- None;
  queue.length <- 0

let iter f queue =
  let rec loop node =
    match node with
    | None -> ()
    | Some n ->
        f n.value;
        loop n.next
  in
  loop queue.front

let fold f queue acc =
  let result = ref acc in
  let rec loop node =
    match node with
    | None -> ()
    | Some n ->
        result := f n.value !result;
        loop n.next
  in
  loop queue.front;
  !result

let to_list queue =
  let result = ref [] in
  let rec loop node =
    match node with
    | None -> ()
    | Some n ->
        result := n.value :: !result;
        loop n.next
  in
  loop queue.front;
  List.rev !result

let contains queue value =
  let found = ref false in
  let rec loop node =
    match node with
    | None -> ()
    | Some n -> if n.value = value then found := true else loop n.next
  in
  loop queue.front;
  !found

let append queue1 queue2 =
  iter (enqueue queue1) queue2;
  clear queue2

let transfer ~src ~dst =
  match src.front with
  | None -> ()
  | Some _ ->
      (match dst.back with
      | None ->
          (* dst is empty, just move everything *)
          dst.front <- src.front;
          dst.back <- src.back;
          dst.length <- src.length
      | Some back_node ->
          (* dst has items, append src to the end *)
          back_node.next <- src.front;
          dst.back <- src.back;
          dst.length <- dst.length + src.length);
      (* Clear src queue *)
      src.front <- None;
      src.back <- None;
      src.length <- 0

let of_list elements =
  let queue = create () in
  List.iter (enqueue queue) elements;
  queue

let to_mut_iter : type item. item t -> item Iter.MutIterator.t =
 fun queue ->
  let module QueueIter = struct
    type state = item t
    type nonrec item = item

    let next queue = dequeue queue
    let size queue = len queue

    let clone queue =
      let queue2 = with_capacity (len queue) in
      iter (enqueue queue2) queue;
      queue2
  end in
  Iter.MutIterator.make (module QueueIter) queue
