open Kernel

type 'a t = {
  mutable value: 'a;
}

let create = fun value -> { value }

let get = fun cell -> cell.value

let ( ! ) = get

let set = fun cell value -> cell.value <- value

let ( := ) = set

let update = fun cell f -> cell.value <- f cell.value

let incr = fun cell -> cell.value <- Int.succ cell.value

let decr = fun cell -> cell.value <- Int.pred cell.value

let replace = fun cell new_value ->
  let old_value = cell.value in
  cell.value <- new_value;
  old_value

let take = fun cell ~default ->
  let old_value = cell.value in
  cell.value <- default;
  old_value

let swap = fun left right ->
  let temp = left.value in
  left.value <- right.value;
  right.value <- temp

let compare_and_swap = fun cell expected new_value ->
  if cell.value = expected then
    (
      cell.value <- new_value;
      true
    )
  else
    false

let equal = fun left right -> left.value = right.value
