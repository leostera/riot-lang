(* Constructor patterns in match *)

let f x =
  match x with
  | Some y -> y
  | None -> 0

let g x =
  match x with
  | Some y -> y
  | None -> 0
