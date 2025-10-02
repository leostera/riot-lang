(** Common types re-exported from Stdlib for use in nostdlib packages *)

type ('value, 'error) result = ('value, 'error) Result.t = Ok of 'value | Error of 'error
type 'a option = 'a Option.t = None | Some of 'a

(* Format types needed for Format.formatter *)
type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'd) Stdlib.format4
type ('a, 'b, 'c, 'd, 'e, 'f) format6 = ('a, 'b, 'c, 'd, 'e, 'f) Stdlib.format6

(* Reference type and operations *)
type 'a ref = 'a Stdlib.ref = { mutable contents : 'a }
let ref = Stdlib.ref
let ( ! ) = Stdlib.( ! )
let ( := ) = Stdlib.( := )
let incr = Stdlib.incr
let decr = Stdlib.decr

(* Comparison operators *)
let ( = ) = Stdlib.( = )
let ( <> ) = Stdlib.( <> )
let ( < ) = Stdlib.( < )
let ( > ) = Stdlib.( > )
let ( <= ) = Stdlib.( <= )
let ( >= ) = Stdlib.( >= )
let compare = Stdlib.compare
let min = Stdlib.min
let max = Stdlib.max

(* Exception handling *)
let raise = Stdlib.raise
let raise_notrace = Stdlib.raise_notrace
let failwith = Stdlib.failwith
let invalid_arg = Stdlib.invalid_arg

(* Integer arithmetic *)
let ( + ) = Stdlib.( + )
let ( - ) = Stdlib.( - )
let ( * ) = Stdlib.( * )
let ( / ) = Stdlib.( / )
let ( mod ) = Stdlib.( mod )
let ( ~- ) = Stdlib.( ~- )
let ( ~+ ) = Stdlib.( ~+ )
let abs = Stdlib.abs
let succ = Stdlib.succ
let pred = Stdlib.pred

(* Float arithmetic *)
let ( +. ) = Stdlib.( +. )
let ( -. ) = Stdlib.( -. )
let ( *. ) = Stdlib.( *. )
let ( /. ) = Stdlib.( /. )
let ( ~-. ) = Stdlib.( ~-. )
let ( ~+. ) = Stdlib.( ~+. )

(* Boolean operations *)
let not = Stdlib.not
let ( && ) = Stdlib.( && )
let ( || ) = Stdlib.( || )
