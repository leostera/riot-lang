open Kernel

let panic = Kernel.SystemError.panic

module Array = Kernel.Array

type 'a box = { mutable value: 'a }

let box = fun value -> { value }

type 'a t = {
  mutable data: 'a option array;
  mutable front: int;
  mutable back: int;
  mutable size: int;
}

let get_slot = fun deque index -> Array.get_unchecked deque.data ~at:index

let set_slot = fun deque index value -> Array.set_unchecked deque.data ~at:index ~value

let create = fun () ->
  {
    data = Array.make ~count:16 ~value:None;
    front = 0;
    back = 0;
    size = 0
  }

let with_capacity = fun ~size ->
  {
    data = Array.make ~count:(Int.max 1 size) ~value:None;
    front = 0;
    back = 0;
    size = 0
  }

let length = fun deque -> deque.size

let is_empty = fun deque -> deque.size = 0

let capacity = fun deque -> Array.length deque.data

let resize = fun deque ->
  let old_capacity = Array.length deque.data in
  let new_capacity = old_capacity * 2 in
  let new_data = Array.make ~count:new_capacity ~value:None in
  for i = 0 to deque.size - 1 do
    let old_index = (deque.front + i) mod old_capacity in Array.set_unchecked new_data ~at:i ~value:(get_slot deque old_index)
  done;
  deque.data <- new_data;
  deque.front <- 0;
  deque.back <- deque.size

let push_back = fun deque ~value ->
  if deque.size = Array.length deque.data then
    resize deque;
  set_slot deque deque.back (Some value);
  deque.back <- (deque.back + 1) mod Array.length deque.data;
  deque.size <- deque.size + 1

let push_front = fun deque ~value ->
  if deque.size = Array.length deque.data then
    resize deque;
  deque.front <- (deque.front - 1 + Array.length deque.data) mod Array.length deque.data;
  set_slot deque deque.front (Some value);
  deque.size <- deque.size + 1

let pop_back = fun deque ->
  if deque.size = 0 then
    None
  else
    (
      deque.back <- (deque.back - 1 + Array.length deque.data) mod Array.length deque.data;
      let value = get_slot deque deque.back in
      set_slot deque deque.back None;
      deque.size <- deque.size - 1;
      value
    )

let pop_front = fun deque ->
  if deque.size = 0 then
    None
  else
    let value = get_slot deque deque.front in set_slot deque deque.front None;
  deque.front <- (deque.front + 1) mod Array.length deque.data;
  deque.size <- deque.size - 1;
  value

let front = fun deque ->
  if deque.size = 0 then
    None
  else get_slot deque deque.front

let back = fun deque ->
  if deque.size = 0 then
    None
  else
    let back_index = (deque.back - 1 + Array.length deque.data) mod Array.length deque.data in get_slot deque back_index

let get = fun deque ~at ->
  if at < 0 || at >= deque.size then
    None
  else
    let actual_index = (deque.front + at) mod Array.length deque.data in get_slot deque actual_index

let insert = fun deque ~at ~value ->
  if at < 0 || at > deque.size then
    panic "Index out of bounds"
  else
    if at = 0 then
      push_front deque ~value
    else
      if at = deque.size then
        push_back deque ~value
      else
        if at <= deque.size / 2 then
          (
            push_front deque ~value;
            for i = 0 to at - 1 do
              let curr_idx = (deque.front + i) mod Array.length deque.data in
              let next_idx = (deque.front + i + 1) mod Array.length deque.data in set_slot deque curr_idx (get_slot deque next_idx)
            done;
            let target_idx = (deque.front + at) mod Array.length deque.data in set_slot deque target_idx (Some value)
          )
        else
          (
            push_back deque ~value;
            for i = deque.size - 2 downto at do
              let curr_idx = (deque.front + i) mod Array.length deque.data in
              let next_idx = (deque.front + i + 1) mod Array.length deque.data in set_slot deque next_idx (get_slot deque curr_idx)
            done;
            let target_idx = (deque.front + at) mod Array.length deque.data in set_slot deque target_idx (Some value)
          )

let remove = fun deque ~at ->
  if at < 0 || at >= deque.size then
    None
  else
    let actual_index = (deque.front + at) mod Array.length deque.data in
    let value = get_slot deque actual_index in
    if at <= deque.size / 2 then
      (
        for i = at downto 1 do
          let curr_idx = (deque.front + i) mod Array.length deque.data in
          let prev_idx = (deque.front + i - 1) mod Array.length deque.data in set_slot deque curr_idx (get_slot deque prev_idx)
        done;
        set_slot deque deque.front None;
        deque.front <- (deque.front + 1) mod Array.length deque.data
      )
    else
      (
        for i = at to deque.size - 2 do
          let curr_idx = (deque.front + i) mod Array.length deque.data in
          let next_idx = (deque.front + i + 1) mod Array.length deque.data in set_slot deque curr_idx (get_slot deque next_idx)
        done;
        deque.back <- (deque.back - 1 + Array.length deque.data) mod Array.length deque.data;
        set_slot deque deque.back None
      );
  deque.size <- deque.size - 1;
  value

let clear = fun deque ->
  for i = 0 to Array.length deque.data - 1 do set_slot deque i None done;
  deque.front <- 0;
  deque.back <- 0;
  deque.size <- 0

let for_each = fun deque ~fn ->
  for i = 0 to deque.size - 1 do
    let index = (deque.front + i) mod Array.length deque.data in
    match get_slot deque index with
    | Some value -> fn value
    | None -> ()
  done

let fold_left = fun deque ~init ~fn ->
  let result = box init in
  for i = 0 to deque.size - 1 do
    let index = (deque.front + i) mod Array.length deque.data in
    match get_slot deque index with
    | Some value -> result.value <- fn result.value value
    | None -> ()
  done;
  result.value

let to_list = fun deque ->
  let result = box [] in
  for i = deque.size - 1 downto 0 do
    let index = (deque.front + i) mod Array.length deque.data in
    match get_slot deque index with
    | Some value -> result.value <- value :: result.value
    | None -> ()
  done;
  result.value

let contains = fun deque ~value ->
  let found = box false in
  for i = 0 to deque.size - 1 do
    let index = (deque.front + i) mod Array.length deque.data in
    match get_slot deque index with
    | Some v when v = value -> found.value <- true
    | _ -> ()
  done;
  found.value

let append = fun deque other ->
  for_each other ~fn:(
    fun value -> push_back deque ~value
  );
  clear other

let split_off = fun deque ~at ->
  if at < 0 || at > deque.size then
    panic "Index out of bounds"
  else
    let new_deque = create () in
    let elements_to_move = deque.size - at in
    for _ = 1 to elements_to_move do
      match pop_back deque with
      | Some value -> push_front new_deque ~value
      | None -> ()
    done;
  new_deque

let from_list = fun elements ->
  let deque = create () in
  List.for_each elements ~fn:(
    fun value -> push_back deque ~value
  );
  deque

let iter: type v. v t -> v Iter.Iterator.t = fun deque ->
  let module DequeIter = struct
    type state = { deque: v t; idx: int }

    type nonrec item = v

    let next = fun state ->
      match get state.deque ~at:state.idx with
      | None -> None, state
      | Some value -> Some value, { state with idx = state.idx + 1 }

    let size = fun state -> max 0 (length state.deque - state.idx)
  end in
  Iter.Iterator.make (module DequeIter) { deque; idx = 0 }

let mut_iter: type v. v t -> v Iter.MutIterator.t = fun deque ->
  let module DequeIter = struct
    type state = v t

    type item = v

    let next = fun deque -> pop_front deque

    let size = fun deque -> length deque

    let clone = fun deque ->
      let deque2 = with_capacity ~size:(length deque) in
      for_each deque ~fn:(
        fun value -> push_back deque2 ~value
      );
      deque2
  end in
  Iter.MutIterator.make (module DequeIter) deque
