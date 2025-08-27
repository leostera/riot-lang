(** Date and time utilities *)

type t = float

let now = Unix.gettimeofday
let to_float x = x
let localtime = Unix.localtime
let gmtime = Unix.gmtime
