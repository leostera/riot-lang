type 'a t = {
  mutable data : 'a option array;
  mutable front : int;
  mutable back : int;
  mutable size : int;
}

let create () = { data = Array.make 16 None; front = 0; back = 0; size = 0 }

let with_capacity capacity =
  { data = Array.make (max 1 capacity) None; front = 0; back = 0; size = 0 }

let len deque = deque.size
let is_empty deque = deque.size = 0
let capacity deque = Array.length deque.data

let resize deque =
  let old_capacity = Array.length deque.data in
  let new_capacity = old_capacity * 2 in
  let new_data = Array.make new_capacity None in

  for i = 0 to deque.size - 1 do
    let old_index = (deque.front + i) mod old_capacity in
    new_data.(i) <- deque.data.(old_index)
  done;

  deque.data <- new_data;
  deque.front <- 0;
  deque.back <- deque.size

let push_back deque value =
  if deque.size = Array.length deque.data then resize deque;
  deque.data.(deque.back) <- Some value;
  deque.back <- (deque.back + 1) mod Array.length deque.data;
  deque.size <- deque.size + 1

let push_front deque value =
  if deque.size = Array.length deque.data then resize deque;
  deque.front <-
    (deque.front - 1 + Array.length deque.data) mod Array.length deque.data;
  deque.data.(deque.front) <- Some value;
  deque.size <- deque.size + 1

let pop_back deque =
  if deque.size = 0 then None
  else (
    deque.back <-
      (deque.back - 1 + Array.length deque.data) mod Array.length deque.data;
    let value = deque.data.(deque.back) in
    deque.data.(deque.back) <- None;
    deque.size <- deque.size - 1;
    value)

let pop_front deque =
  if deque.size = 0 then None
  else
    let value = deque.data.(deque.front) in
    deque.data.(deque.front) <- None;
    deque.front <- (deque.front + 1) mod Array.length deque.data;
    deque.size <- deque.size - 1;
    value

let front deque = if deque.size = 0 then None else deque.data.(deque.front)

let back deque =
  if deque.size = 0 then None
  else
    let back_index =
      (deque.back - 1 + Array.length deque.data) mod Array.length deque.data
    in
    deque.data.(back_index)

let get deque index =
  if index < 0 || index >= deque.size then None
  else
    let actual_index = (deque.front + index) mod Array.length deque.data in
    deque.data.(actual_index)

let insert deque index value =
  if index < 0 || index > deque.size then invalid_arg "Index out of bounds"
  else if index = 0 then push_front deque value
  else if index = deque.size then push_back deque value
  else if index <= deque.size / 2 then (
    push_front deque value;
    for i = 0 to index - 1 do
      let curr_idx = (deque.front + i) mod Array.length deque.data in
      let next_idx = (deque.front + i + 1) mod Array.length deque.data in
      deque.data.(curr_idx) <- deque.data.(next_idx)
    done;
    let target_idx = (deque.front + index) mod Array.length deque.data in
    deque.data.(target_idx) <- Some value)
  else (
    push_back deque value;
    for i = deque.size - 2 downto index do
      let curr_idx = (deque.front + i) mod Array.length deque.data in
      let next_idx = (deque.front + i + 1) mod Array.length deque.data in
      deque.data.(next_idx) <- deque.data.(curr_idx)
    done;
    let target_idx = (deque.front + index) mod Array.length deque.data in
    deque.data.(target_idx) <- Some value)

let remove deque index =
  if index < 0 || index >= deque.size then None
  else
    let actual_index = (deque.front + index) mod Array.length deque.data in
    let value = deque.data.(actual_index) in

    if index <= deque.size / 2 then (
      for i = index downto 1 do
        let curr_idx = (deque.front + i) mod Array.length deque.data in
        let prev_idx = (deque.front + i - 1) mod Array.length deque.data in
        deque.data.(curr_idx) <- deque.data.(prev_idx)
      done;
      deque.data.(deque.front) <- None;
      deque.front <- (deque.front + 1) mod Array.length deque.data)
    else (
      for i = index to deque.size - 2 do
        let curr_idx = (deque.front + i) mod Array.length deque.data in
        let next_idx = (deque.front + i + 1) mod Array.length deque.data in
        deque.data.(curr_idx) <- deque.data.(next_idx)
      done;
      deque.back <-
        (deque.back - 1 + Array.length deque.data) mod Array.length deque.data;
      deque.data.(deque.back) <- None);

    deque.size <- deque.size - 1;
    value

let clear deque =
  for i = 0 to Array.length deque.data - 1 do
    deque.data.(i) <- None
  done;
  deque.front <- 0;
  deque.back <- 0;
  deque.size <- 0

let iter f deque =
  for i = 0 to deque.size - 1 do
    let index = (deque.front + i) mod Array.length deque.data in
    match deque.data.(index) with Some value -> f value | None -> ()
  done

let fold f deque acc =
  let result = ref acc in
  for i = 0 to deque.size - 1 do
    let index = (deque.front + i) mod Array.length deque.data in
    match deque.data.(index) with
    | Some value -> result := f value !result
    | None -> ()
  done;
  !result

let to_list deque =
  let result = ref [] in
  for i = deque.size - 1 downto 0 do
    let index = (deque.front + i) mod Array.length deque.data in
    match deque.data.(index) with
    | Some value -> result := value :: !result
    | None -> ()
  done;
  !result

let contains deque value =
  let found = ref false in
  for i = 0 to deque.size - 1 do
    let index = (deque.front + i) mod Array.length deque.data in
    match deque.data.(index) with
    | Some v when v = value -> found := true
    | _ -> ()
  done;
  !found

let append deque1 deque2 =
  iter (push_back deque1) deque2;
  clear deque2

let split_off deque index =
  if index < 0 || index > deque.size then invalid_arg "Index out of bounds"
  else
    let new_deque = create () in
    let elements_to_move = deque.size - index in

    for _ = 1 to elements_to_move do
      match pop_back deque with
      | Some value -> push_front new_deque value
      | None -> ()
    done;

    new_deque

let of_list elements =
  let deque = create () in
  List.iter (push_back deque) elements;
  deque

let into_iter : type v. v t -> v Iter.Iterator.t =
 fun deque ->
  let module DequeIter = struct
    type state = { deque : v t; idx : int }
    type nonrec item = v

    let next state =
      match get state.deque state.idx with
      | None -> (None, state)
      | Some value -> (Some value, { state with idx = state.idx + 1 })

    let size state = max 0 (len state.deque - state.idx)
  end in
  Iter.Iterator.make (module DequeIter) { deque; idx = 0 }

let to_mut_iter : type v. v t -> v Iter.MutIterator.t =
 fun deque ->
  let module DequeIter = struct
    type state = v t
    type item = v

    let next deque = pop_front deque
    let size deque = len deque

    let clone deque =
      let deque2 = with_capacity (len deque) in
      iter (push_back deque2) deque;
      deque2
  end in
  Iter.MutIterator.make (module DequeIter) deque
