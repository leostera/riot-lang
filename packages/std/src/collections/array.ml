open Kernel

type 'value t = 'value array

let make = fun ~count ~value -> Kernel.Array.make ~count ~value

let init = fun ~count ~fn -> Kernel.Array.init ~count ~fn

let length = Kernel.Array.length

let get = fun values ~at -> Kernel.Array.get values ~at

let get_unchecked = fun values ~at -> Kernel.Array.get_unchecked values ~at

let set = fun values ~at ~value -> Kernel.Array.set values ~at ~value

let set_unchecked = fun values ~at ~value -> Kernel.Array.set_unchecked values ~at ~value

let clone = Kernel.Array.clone

let blit = fun values ~src_offset ~dst ~dst_offset ~len ->
  Kernel.Array.blit
    values
    ~src_offset
    ~dst
    ~dst_offset
    ~len

let sub = fun values ~offset ~len -> Kernel.Array.sub values ~offset ~len

let for_each = fun values ~fn -> Kernel.Array.for_each values ~fn

let map = fun values ~fn -> Kernel.Array.map values ~fn

let fold_left = fun values ~init ~fn -> Kernel.Array.fold_left values ~acc:init ~fn:fn

let fold_right = fun values ~init ~fn -> Kernel.Array.fold_right values ~acc:init ~fn:fn

let from_list = Kernel.Array.from_list

let to_list = fun values ->
  let rec loop index acc =
    if index < 0 then
      acc
    else
      loop (index - 1) (get_unchecked values ~at:index :: acc)
  in
  loop (length values - 1) []

let iter: type item. item array -> item Iter.Iterator.t = fun arr ->
  let module ArrayIter = struct
    type state = {
      arr: item array;
      idx: int;
    }

    type nonrec item = item

    let next = fun state ->
      if state.idx >= length state.arr then
        (None, state)
      else
        let value = get_unchecked state.arr ~at:state.idx in
        (Some value, { state with idx = state.idx + 1 })

    let size = fun state -> length state.arr - state.idx
  end in
  Iter.Iterator.make (module ArrayIter) { arr; idx = 0 }

let mut_iter: type item. item array -> item Iter.MutIterator.t = fun arr ->
  let module ArrayMutIter = struct
    type state = {
      arr: item array;
      mutable idx: int;
    }

    type nonrec item = item

    let next = fun state ->
      if state.idx >= length state.arr then
        None
      else
        let value = get_unchecked state.arr ~at:state.idx in
        state.idx <- state.idx + 1;
      Some value

    let size = fun state -> length state.arr - state.idx

    let clone = fun state -> { arr = clone state.arr; idx = state.idx }
  end in
  Iter.MutIterator.make (module ArrayMutIter) { arr; idx = 0 }

module Syntax = struct
  let get = fun values at -> get_unchecked values ~at

  let set = fun values at value -> set_unchecked values ~at ~value
end
