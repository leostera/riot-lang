(** Common types re-exported from Stdlib for use in nostdlib packages *)

type ('value, 'error) result = ('value, 'error) Result.t =
  | Ok of 'value
  | Error of 'error

type 'a option = 'a Option.t = None | Some of 'a

(* Format types needed for Format.formatter *)
type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'd) Stdlib.format4
type ('a, 'b, 'c, 'd, 'e, 'f) format6 = ('a, 'b, 'c, 'd, 'e, 'f) Stdlib.format6

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
exception Exit

val raise : exn -> 'a
val raise_notrace : exn -> 'a
val failwith : string -> 'a
val invalid_arg : string -> 'a
val exit : int -> 'a

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
val max_int : int
val min_int : int

(* Bitwise operations *)
val ( land ) : int -> int -> int
val ( lor ) : int -> int -> int
val ( lxor ) : int -> int -> int
val lnot : int -> int
val ( lsl ) : int -> int -> int
val ( lsr ) : int -> int -> int
val ( asr ) : int -> int -> int

(* Float arithmetic *)
val ( +. ) : float -> float -> float
val ( -. ) : float -> float -> float
val ( *. ) : float -> float -> float
val ( /. ) : float -> float -> float
val ( ~-. ) : float -> float
val ( ~+. ) : float -> float
val floor : float -> float
val ceil : float -> float
val sqrt : float -> float
val exp : float -> float
val log : float -> float
val log10 : float -> float
val cos : float -> float
val sin : float -> float
val tan : float -> float
val acos : float -> float
val asin : float -> float
val atan : float -> float
val atan2 : float -> float -> float
val cosh : float -> float
val sinh : float -> float
val tanh : float -> float
val acosh : float -> float
val asinh : float -> float
val atanh : float -> float
val expm1 : float -> float
val log1p : float -> float
val copysign : float -> float -> float
val mod_float : float -> float -> float
val frexp : float -> float * int
val ldexp : float -> int -> float
val modf : float -> float * float
val float_of_int : int -> float
val int_of_float : float -> int
val truncate : float -> int
val string_of_int : int -> string
val string_of_float : float -> string
val int_of_string : string -> int
val int_of_string_opt : string -> int option
val float_of_string : string -> float
val float_of_string_opt : string -> float option
val string_of_bool : bool -> string
val bool_of_string : string -> bool
val bool_of_string_opt : string -> bool option
val ( ^ ) : string -> string -> string
val ( @ ) : 'a list -> 'a list -> 'a list
val infinity : float
val neg_infinity : float
val nan : float
val max_float : float
val min_float : float
val epsilon_float : float

type fpclass = Stdlib.fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

val classify_float : float -> fpclass

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

val format : ('a, unit, string, string) format4 -> 'a
(** Format string helper - alias for format *)

val print : ('a, unit, string, unit) format4 -> 'a
(** Print to stdout with immediate flush *)

val println : ('a, unit, string, unit) format4 -> 'a
(** Print to stdout with newline and immediate flush *)

val eprint : ('a, unit, string, unit) format4 -> 'a
(** Print to stderr with immediate flush *)

val eprintln : ('a, unit, string, unit) format4 -> 'a
(** Print to stderr with newline and immediate flush *)
