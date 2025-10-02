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

(* Exception handling *)
val raise : exn -> 'a
val raise_notrace : exn -> 'a
val failwith : string -> 'a
val invalid_arg : string -> 'a

(* Integer arithmetic *)
val ( + ) : int -> int -> int
val ( - ) : int -> int -> int
val ( * ) : int -> int -> int
val ( / ) : int -> int -> int
val ( mod ) : int -> int -> int
val ( ~- ) : int -> int
val ( ~+ ) : int -> int
val abs : int -> int
val succ : int -> int
val pred : int -> int

(* Float arithmetic *)
val ( +. ) : float -> float -> float
val ( -. ) : float -> float -> float
val ( *. ) : float -> float -> float
val ( /. ) : float -> float -> float
val ( ~-. ) : float -> float
val ( ~+. ) : float -> float

(* Boolean operations *)
val not : bool -> bool
val ( && ) : bool -> bool -> bool
val ( || ) : bool -> bool -> bool

(* Utility functions *)
val ignore : 'a -> unit
val ( |> ) : 'a -> ('a -> 'b) -> 'b
val ( @@ ) : ('a -> 'b) -> 'a -> 'b
val fst : 'a * 'b -> 'a
val snd : 'a * 'b -> 'b
