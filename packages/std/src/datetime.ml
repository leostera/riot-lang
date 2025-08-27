(** Date and time utilities *)

let now () = Unix.gettimeofday ()
let localtime timestamp = Unix.localtime timestamp
let gmtime timestamp = Unix.gmtime timestamp
