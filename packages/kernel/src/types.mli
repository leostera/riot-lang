(** Common types re-exported from Stdlib for use in nostdlib packages *)

type ('value, 'error) result = ('value, 'error) Result.t = Ok of 'value | Error of 'error
type 'a option = 'a Option.t = None | Some of 'a

(* Format types needed for Format.formatter *)
type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'd) Stdlib.format4
type ('a, 'b, 'c, 'd, 'e, 'f) format6 = ('a, 'b, 'c, 'd, 'e, 'f) Stdlib.format6

(* Reference type and operations *)
type 'a ref = 'a Stdlib.ref = { mutable contents : 'a }
val ref : 'a -> 'a ref
val ( ! ) : 'a ref -> 'a
val ( := ) : 'a ref -> 'a -> unit
val incr : int ref -> unit
val decr : int ref -> unit

(* Comparison operators *)
val ( = ) : 'a -> 'a -> bool
val ( <> ) : 'a -> 'a -> bool
val ( < ) : 'a -> 'a -> bool
val ( > ) : 'a -> 'a -> bool
val ( <= ) : 'a -> 'a -> bool
val ( >= ) : 'a -> 'a -> bool
val compare : 'a -> 'a -> int
val min : 'a -> 'a -> 'a
val max : 'a -> 'a -> 'a
