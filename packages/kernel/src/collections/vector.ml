open Global0

type 'a t = {
  mutable data: 'a array;
  mutable length: int;
}

let create = fun () -> { data = [||]; length = 0 }

let with_capacity = fun capacity -> { data = Array.make capacity (Obj.magic 0); length = 0 }

let capacity = fun vector -> Array.length vector.data

let len = fun vector -> vector.length

let is_empty = fun vector -> vector.length = 0

let resize = fun vector new_capacity ->
  let old_data = vector.data in
  let new_data = Array.make new_capacity (Obj.magic 0) in
  Array.blit old_data 0 new_data 0 vector.length;
  vector.data <- new_data

let ensure_capacity = fun vector required_capacity ->
  let current_capacity = capacity vector in
  if current_capacity < required_capacity then
    let new_capacity = max required_capacity (current_capacity * 2) in
    let new_capacity = max new_capacity 4 in
    resize vector new_capacity

let reserve = fun vector additional ->
  if additional < 0 then
    panic "additional capacity must be non-negative"
  else
    ensure_capacity vector (vector.length + additional)

let push = fun vector value ->
  ensure_capacity vector (vector.length + 1);
  Array.unsafe_set vector.data vector.length value;
  vector.length <- vector.length + 1

let pop = fun vector ->
  if vector.length = 0 then
    None
  else (
    vector.length <- vector.length - 1;
    Some (Array.unsafe_get vector.data vector.length)
  )

let get = fun vector index ->
  if index >= 0 && index < vector.length then
    Some (Array.unsafe_get vector.data index)
  else
    None

let get_unchecked = fun vector index ->
  Array.unsafe_get vector.data index

let set = fun vector index value ->
  if index < 0 || index >= vector.length then
    panic "Index out of bounds"
  else
    Array.unsafe_set vector.data index value

let set_unchecked = fun vector index value ->
  Array.unsafe_set vector.data index value

let insert = fun vector index value ->
  if index < 0 || index > vector.length then
    panic "Index out of bounds"
  else (
    ensure_capacity vector (vector.length + 1);
    Array.blit vector.data index vector.data (index + 1) (vector.length - index);
    Array.unsafe_set vector.data index value;
    vector.length <- vector.length + 1
  )

let remove = fun vector index ->
  if index < 0 || index >= vector.length then
    None
  else
    (
      let value = Array.unsafe_get vector.data index in
      Array.blit vector.data (index + 1) vector.data index (vector.length - index - 1);
      vector.length <- vector.length - 1;
      Some value
    )

let clear = fun vector -> vector.length <- 0

let to_array = fun vector ->
  Array.sub vector.data 0 vector.length

let iter = fun f vector ->
  let data = vector.data in
  for i = 0 to vector.length - 1 do
    f (Array.unsafe_get data i)
  done

let append = fun vector1 vector2 ->
  if vector2.length > 0 then
    (
      reserve vector1 vector2.length;
      Array.blit vector2.data 0 vector1.data vector1.length vector2.length;
      vector1.length <- vector1.length + vector2.length;
      vector2.length <- 0
    )

let split_off = fun vector index ->
  if index < 0 || index > vector.length then
    panic "Index out of bounds"
  else
    (
      let elements_to_move = vector.length - index in
      let new_vector = with_capacity elements_to_move in
      Array.blit vector.data index new_vector.data 0 elements_to_move;
      new_vector.length <- elements_to_move;
      vector.length <- index;
      new_vector
    )

let sort = fun vector ->
  if vector.length > 0 then
    (
      let temp_array = Array.sub vector.data 0 vector.length in
      Array.sort compare temp_array;
      for i = 0 to vector.length - 1 do
        vector.data.(i) <- temp_array.(i)
      done
    )

let sort_by = fun vector compare_fn ->
  if vector.length > 0 then
    (
      let temp_array = Array.sub vector.data 0 vector.length in
      Array.sort compare_fn temp_array;
      for i = 0 to vector.length - 1 do
        vector.data.(i) <- temp_array.(i)
      done
    )

let reverse = fun vector ->
  for i = 0 to (vector.length - 1) / 2 do
    let j = vector.length - 1 - i in
    let temp = Array.unsafe_get vector.data i in
    Array.unsafe_set vector.data i (Array.unsafe_get vector.data j);
    Array.unsafe_set vector.data j temp
  done

let first = fun vector ->
  if vector.length = 0 then
    None
  else
    Some (Array.unsafe_get vector.data 0)

let last = fun vector ->
  if vector.length = 0 then
    None
  else
    Some (Array.unsafe_get vector.data (vector.length - 1))

let of_list = fun elements ->
  let data = Array.of_list elements in
  { data; length = Array.length data }

let into_iter: type item. item t -> item Iter.Iterator.t = fun vector ->
  let module VecIter = struct
    type state = {
      vec: item t;
      pos: int;
    }

    type nonrec item = item

    let next = fun state ->
      if state.pos >= state.vec.length then
        (None, state)
      else
        let item = Array.unsafe_get state.vec.data state.pos in
        (Some item, { state with pos = state.pos + 1 })

    let size = fun state -> max 0 (state.vec.length - state.pos)
  end in
  Iter.Iterator.make (module VecIter) { VecIter.vec = vector; pos = 0 }

let to_mut_iter: type item. item t -> item Iter.MutIterator.t = fun vector ->
  let module VecIter = struct
    type state = {
      vec: item t;
      mutable pos: int;
    }

    type nonrec item = item

    let next = fun state ->
      if state.pos >= state.vec.length then
        None
      else
        let item = Array.unsafe_get state.vec.data state.pos in
        state.pos <- state.pos + 1;
        Some item

    let size = fun state -> max 0 (state.vec.length - state.pos)

    let clone = fun state -> { vec = state.vec; pos = state.pos }
  end in
  Iter.MutIterator.make (module VecIter) { VecIter.vec = vector; pos = 0 }
