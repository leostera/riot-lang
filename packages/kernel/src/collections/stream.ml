include Stdlib.Seq

let into_iter : type item. item t -> item Iter.Iterator.t =
 fun seq ->
  let module StreamIter = struct
    type state = item t
    type nonrec item = item

    let next state =
      match state () with
      | Nil -> (None, state)
      | Cons (value, rest) -> (Some value, rest)

    let size _state = 0 (* Unknown size for lazy sequences *)
  end in
  Iter.Iterator.make (module StreamIter) seq

let to_mut_iter : type item. item t -> item Iter.MutIterator.t =
 fun seq ->
  let module StreamMutIter = struct
    type state = { mutable seq : item t }
    type nonrec item = item

    let next state =
      match state.seq () with
      | Nil -> None
      | Cons (value, rest) ->
          state.seq <- rest;
          Some value

    let size _state = 0 (* Unknown size for lazy sequences *)
    let clone state = { seq = state.seq }
  end in
  Iter.MutIterator.make (module StreamMutIter) { seq }
