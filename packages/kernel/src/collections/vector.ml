type 'a t = { mutable data : 'a array; mutable length : int }

let create () = { data = [||]; length = 0 }

let with_capacity capacity =
  { data = Array.make capacity (Obj.magic 0); length = 0 }

let capacity vector = Array.length vector.data
let len vector = vector.length
let is_empty vector = vector.length = 0

let resize vector new_capacity =
  let old_data = vector.data in
  let new_data = Array.make new_capacity (Obj.magic 0) in
  for i = 0 to vector.length - 1 do
    new_data.(i) <- old_data.(i)
  done;
  vector.data <- new_data

let ensure_capacity vector required_capacity =
  if Array.length vector.data < required_capacity then
    let new_capacity = max required_capacity (Array.length vector.data * 2) in
    let new_capacity = max new_capacity 4 in
    resize vector new_capacity

let push vector value =
  ensure_capacity vector (vector.length + 1);
  vector.data.(vector.length) <- value;
  vector.length <- vector.length + 1

let pop vector =
  if vector.length = 0 then None
  else (
    vector.length <- vector.length - 1;
    Some vector.data.(vector.length))

let get vector index =
  if index >= 0 && index < vector.length then Some vector.data.(index) else None

let set vector index value =
  if index < 0 || index >= vector.length then invalid_arg "Index out of bounds"
  else vector.data.(index) <- value

let insert vector index value =
  if index < 0 || index > vector.length then invalid_arg "Index out of bounds"
  else (
    ensure_capacity vector (vector.length + 1);
    for i = vector.length downto index + 1 do
      vector.data.(i) <- vector.data.(i - 1)
    done;
    vector.data.(index) <- value;
    vector.length <- vector.length + 1)

let remove vector index =
  if index < 0 || index >= vector.length then None
  else
    let value = vector.data.(index) in
    for i = index to vector.length - 2 do
      vector.data.(i) <- vector.data.(i + 1)
    done;
    vector.length <- vector.length - 1;
    Some value

let clear vector = vector.length <- 0

let iter f vector =
  for i = 0 to vector.length - 1 do
    f vector.data.(i)
  done

let fold f vector acc =
  let result = ref acc in
  for i = 0 to vector.length - 1 do
    result := f vector.data.(i) !result
  done;
  !result

let to_list vector =
  let result = ref [] in
  for i = vector.length - 1 downto 0 do
    result := vector.data.(i) :: !result
  done;
  !result

let contains vector value =
  let found = ref false in
  for i = 0 to vector.length - 1 do
    if vector.data.(i) = value then found := true
  done;
  !found

let append vector1 vector2 =
  for i = 0 to vector2.length - 1 do
    push vector1 vector2.data.(i)
  done;
  vector2.length <- 0

let split_off vector index =
  if index < 0 || index > vector.length then invalid_arg "Index out of bounds"
  else
    let new_vector = create () in
    let elements_to_move = vector.length - index in
    ensure_capacity new_vector elements_to_move;

    for i = index to vector.length - 1 do
      new_vector.data.(i - index) <- vector.data.(i)
    done;

    new_vector.length <- elements_to_move;
    vector.length <- index;
    new_vector

let sort vector =
  if vector.length > 0 then (
    let temp_array = Array.sub vector.data 0 vector.length in
    Array.sort compare temp_array;
    for i = 0 to vector.length - 1 do
      vector.data.(i) <- temp_array.(i)
    done)

let sort_by vector compare_fn =
  if vector.length > 0 then (
    let temp_array = Array.sub vector.data 0 vector.length in
    Array.sort compare_fn temp_array;
    for i = 0 to vector.length - 1 do
      vector.data.(i) <- temp_array.(i)
    done)

let reverse vector =
  for i = 0 to (vector.length - 1) / 2 do
    let j = vector.length - 1 - i in
    let temp = vector.data.(i) in
    vector.data.(i) <- vector.data.(j);
    vector.data.(j) <- temp
  done

let first vector = if vector.length = 0 then None else Some vector.data.(0)

let last vector =
  if vector.length = 0 then None else Some vector.data.(vector.length - 1)

let of_list elements =
  let vector = with_capacity (List.length elements) in
  List.iter (push vector) elements;
  vector

let into_iter : type item. item t -> item Iter.Iterator.t =
 fun vector ->
  let module VecIter = struct
    type state = { vec : item t; pos : int }
    type nonrec item = item

    let next state =
      if state.pos >= state.vec.length then (None, state)
      else
        let item = state.vec.data.(state.pos) in
        (Some item, { state with pos = state.pos + 1 })

    let size state = max 0 (state.vec.length - state.pos)
  end in
  Iter.Iterator.make (module VecIter) { VecIter.vec = vector; pos = 0 }

let to_mut_iter : type item. item t -> item Iter.MutIterator.t =
 fun vector ->
  let module VecIter = struct
    type state = { vec : item t; mutable pos : int }
    type nonrec item = item

    let next state =
      if state.pos >= state.vec.length then None
      else
        let item = state.vec.data.(state.pos) in
        state.pos <- state.pos + 1;
        Some item

    let size state = max 0 (state.vec.length - state.pos)
    let clone state = { vec = state.vec; pos = state.pos }
  end in
  Iter.MutIterator.make (module VecIter) { VecIter.vec = vector; pos = 0 }
