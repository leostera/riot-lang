(** Time operations for Kernel *)

let time () = Unix.time ()
let gettimeofday () = Unix.gettimeofday ()
let localtime t = Unix.localtime t
let gmtime t = Unix.gmtime t
let mktime tm = Unix.mktime tm
