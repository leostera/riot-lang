open Kernel

type 'value t = {
  mutable data: 'value array;
  mutable length: int;
}

type error =
  | OutOfBounds
  | OutOfBoundsSet of { length: int; at: int }

let create = fun () -> { data = [||]; length = 0 }

let with_capacity = fun ~size ->
  { data = Array.make ~count:(Int.max 0 size) ~value:(dangerously_cast_value 0); length = 0 }

let length = fun vector -> vector.length

let len = length

let is_empty = fun vector ->
  Int.equal vector.length 0

let capacity = fun vector -> Array.length vector.data

let resize = fun vector new_capacity ->
  let new_data = Array.make ~count:new_capacity ~value:(dangerously_cast_value 0) in
  Array.blit vector.data ~src_offset:0 ~dst:new_data ~dst_offset:0 ~len:vector.length;
  vector.data <- new_data

let ensure_capacity = fun vector required_capacity ->
  let current_capacity = capacity vector in
  if current_capacity < required_capacity then
    let grown_capacity =
      if Int.equal current_capacity 0 then
        4
      else
        Int.mul current_capacity 2
    in
    resize vector (Int.max required_capacity grown_capacity)

let reserve = fun vector ~size ->
  if size < 0 then
    Kernel.SystemError.panic "Vector.reserve received a negative size"
  else
    ensure_capacity vector (Int.add vector.length size)

let push = fun vector ~value ->
  ensure_capacity vector (Int.add vector.length 1);
  Array.set_unchecked vector.data ~at:vector.length ~value;
  vector.length <- Int.add vector.length 1

let pop = fun vector ->
  if Int.equal vector.length 0 then
    None
  else (
    vector.length <- Int.sub vector.length 1;
    Some (Array.get_unchecked vector.data ~at:vector.length)
  )

let get = fun vector ~at ->
  if at < 0 || at >= vector.length then
    None
  else
    Some (Array.get_unchecked vector.data ~at)

let get_unchecked = fun vector ~at -> Array.get_unchecked vector.data ~at

let set = fun vector ~at ~value ->
  if at < 0 || at >= vector.length then
    Error (OutOfBoundsSet { length = vector.length; at })
  else (
    Array.set_unchecked vector.data ~at ~value;
    Ok ()
  )

let set_unchecked = fun vector ~at ~value -> Array.set_unchecked vector.data ~at ~value

let insert = fun vector ~at ~value ->
  if at < 0 || at > vector.length then
    Kernel.SystemError.panic "Vector.insert received an out-of-bounds index"
  else (
    ensure_capacity vector (Int.add vector.length 1);
    Array.blit
      vector.data
      ~src_offset:at
      ~dst:vector.data
      ~dst_offset:(Int.add at 1)
      ~len:(Int.sub vector.length at);
    Array.set_unchecked vector.data ~at ~value;
    vector.length <- Int.add vector.length 1
  )

let remove = fun vector ~at ->
  if at < 0 || at >= vector.length then
    None
  else
    let value = Array.get_unchecked vector.data ~at in
    Array.blit
      vector.data
      ~src_offset:(Int.add at 1)
      ~dst:vector.data
      ~dst_offset:at
      ~len:(Int.sub (Int.sub vector.length at) 1);
    vector.length <- Int.sub vector.length 1;
    Some value

let clear = fun vector -> vector.length <- 0

let truncate_unchecked = fun vector ~len -> vector.length <- len

let truncate = fun vector ~len:new_length ->
  if Int.(new_length < 0) || Int.(new_length > len vector) then
    Error OutOfBounds
  else (
    truncate_unchecked vector ~len:new_length;
    Ok ()
  )

let to_array = fun vector -> Array.sub vector.data ~offset:0 ~len:vector.length

let for_each = fun vector ~fn ->
  let rec loop index =
    if index >= vector.length then
      ()
    else (
      fn (Array.get_unchecked vector.data ~at:index);
      loop (Int.add index 1)
    )
  in
  loop 0

let append = fun left right ->
  if right.length > 0 then
    (
      reserve left ~size:right.length;
      Array.blit right.data ~src_offset:0 ~dst:left.data ~dst_offset:left.length ~len:right.length;
      left.length <- Int.add left.length right.length;
      right.length <- 0
    )

let split_off = fun vector ~at ->
  if at < 0 || at > vector.length then
    Kernel.SystemError.panic "Vector.split_off received an out-of-bounds index"
  else
    let moved = Int.sub vector.length at in
    let next = with_capacity ~size:moved in
    Array.blit vector.data ~src_offset:at ~dst:next.data ~dst_offset:0 ~len:moved;
    next.length <- moved;
    vector.length <- at;
    next

let sort_with = fun vector ~compare ->
  let rec shift_left item index =
    if index < 0 then
      0
    else
      let current = Array.get_unchecked vector.data ~at:index in
      match compare current item with
      | Order.LT
      | Order.EQ -> Int.add index 1
      | Order.GT ->
          Array.set_unchecked vector.data ~at:(Int.add index 1) ~value:current;
          shift_left item (Int.sub index 1)
  in
  for index = 1 to Int.sub vector.length 1 do
    let item = Array.get_unchecked vector.data ~at:index in
    let destination = shift_left item (Int.sub index 1) in
    Array.set_unchecked vector.data ~at:destination ~value:item
  done

let sort = fun vector -> sort_with vector ~compare

let sort_by = fun vector ~compare -> sort_with vector ~compare

let reverse = fun vector ->
  for index = 0 to Int.div (Int.sub vector.length 1) 2 do
    let opposite = Int.sub (Int.sub vector.length 1) index in
    let value = Array.get_unchecked vector.data ~at:index in
    Array.set_unchecked vector.data ~at:index ~value:(Array.get_unchecked vector.data ~at:opposite);
    Array.set_unchecked vector.data ~at:opposite ~value
  done

let first = fun vector ->
  if Int.equal vector.length 0 then
    None
  else
    Some (Array.get_unchecked vector.data ~at:0)

let last = fun vector ->
  if Int.equal vector.length 0 then
    None
  else
    Some (Array.get_unchecked vector.data ~at:(Int.sub vector.length 1))

let from_list = fun values ->
  let data = Array.from_list values in
  { data; length = Array.length data }

let iter: type item. item t -> item Iter.Iterator.t = fun vector ->
  let module VecIter = struct
    type state = {
      vector: item t;
      position: int;
    }

    type nonrec item = item

    let next = fun state ->
      if state.position >= state.vector.length then
        (None, state)
      else
        let item = Array.get_unchecked state.vector.data ~at:state.position in
        (Some item, { state with position = Int.add state.position 1 })

    let size = fun state ->
      Int.max 0 (Int.sub state.vector.length state.position)
  end in
  Iter.Iterator.make (module VecIter) { vector; position = 0 }

let mut_iter: type item. item t -> item Iter.MutIterator.t = fun vector ->
  let module VecIter = struct
    type state = {
      vector: item t;
      mutable position: int;
    }

    type nonrec item = item

    let next = fun state ->
      if state.position >= state.vector.length then
        None
      else
        let item = Array.get_unchecked state.vector.data ~at:state.position in
        state.position <- Int.add state.position 1;
        Some item

    let size = fun state ->
      Int.max 0 (Int.sub state.vector.length state.position)

    let clone = fun state -> { vector = state.vector; position = state.position }
  end in
  Iter.MutIterator.make (module VecIter) { vector; position = 0 }
