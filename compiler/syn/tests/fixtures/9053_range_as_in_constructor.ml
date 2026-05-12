(* Character ranges with as binding in constructor patterns *)

let f = function
  | Some ('0' .. '9' as c) -> c
  | None -> '?'

(* Multiple levels *)

let g = function
  | Ok (Some ('a' .. 'z' as c)) -> c
  | _ -> 'x'

(* In match *)

let h x =
  match x with
  | Some ('A' .. 'Z' as upper) -> upper
  | Some ('a' .. 'z' as lower) -> lower
  | _ -> ' '
