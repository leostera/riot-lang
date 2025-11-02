include Stdlib.Array

let into_iter : type item. item array -> item Iter.Iterator.t =
 fun arr ->
  let module ArrayIter = struct
    type state = { arr : item array; idx : int }
    type nonrec item = item

    let next state =
      if state.idx >= length state.arr then (None, state)
      else
        let value = get state.arr state.idx in
        (Some value, { state with idx = state.idx + 1 })

    let size state = length state.arr - state.idx
  end in
  Iter.Iterator.make (module ArrayIter) { arr; idx = 0 }

let to_mut_iter : type item. item array -> item Iter.MutIterator.t =
 fun arr ->
  let module ArrayMutIter = struct
    type state = { arr : item array; mutable idx : int }
    type nonrec item = item

    let next state =
      if state.idx >= length state.arr then None
      else (
        let value = get state.arr state.idx in
        state.idx <- state.idx + 1;
        Some value)

    let size state = length state.arr - state.idx
    let clone state = { arr = copy state.arr; idx = state.idx }
  end in
  Iter.MutIterator.make (module ArrayMutIter) { arr; idx = 0 }
