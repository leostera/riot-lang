(** Basic mutable cell *)

type 'a cell = { mutable value : 'a }
type 'a t = 'a cell

(* Creation *)
let create value = { value }

(* Reading *)
let get cell = cell.value

(* Operators for ref-like syntax *)
let ( ! ) = get

(* Writing *)
let set cell x = cell.value <- x

(* Operators for ref-like syntax *)
let ( := ) = set

(* Updating *)
let update cell f = cell.value <- f cell.value
let incr cell = cell.value <- cell.value + 1
let decr cell = cell.value <- cell.value - 1

let replace cell new_value =
  let old_value = cell.value in
  cell.value <- new_value;
  old_value

(* Taking - useful for option/result types *)
let take cell ~default =
  let old_value = cell.value in
  cell.value <- default;
  old_value

(* Swapping *)
let swap cell1 cell2 =
  let temp = cell1.value in
  cell1.value <- cell2.value;
  cell2.value <- temp

(* Comparison *)
let compare_and_swap cell expected new_value =
  if cell.value = expected then (
    cell.value <- new_value;
    true)
  else false

let equal cell1 cell2 = cell1.value = cell2.value