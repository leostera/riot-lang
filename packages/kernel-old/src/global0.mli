(** Core primitives from Stdlib that don't depend on any other modules *)
(* Re-export basic types *)

include module type of Types

(* Comparison operators *)
val ( = ): 'a -> 'a -> bool

val ( != ): 'a -> 'a -> bool

val ( <> ): 'a -> 'a -> bool

val ptr_eq: 'a -> 'a -> bool

val ptr_not_eq: 'a -> 'a -> bool

val ( < ): 'a -> 'a -> bool

val ( > ): 'a -> 'a -> bool

val ( <= ): 'a -> 'a -> bool

val ( >= ): 'a -> 'a -> bool

val compare: 'a -> 'a -> int

val min: 'a -> 'a -> 'a

val max: 'a -> 'a -> 'a

(* Exception handling *)

exception Exit

val raise: exn -> 'a

val raise_notrace: exn -> 'a

val exit: int -> 'a

(* Integer arithmetic *)
val ( + ): int -> int -> int

val ( - ): int -> int -> int

val ( * ): int -> int -> int

val ( ** ): float -> float -> float

val ( / ): int -> int -> int

val ( mod ): int -> int -> int

val ( ~- ): int -> int

val ( ~+ ): int -> int

val abs: int -> int

val succ: int -> int

val pred: int -> int

val max_int: int

val min_int: int

(* Bitwise operations *)
val ( land ): int -> int -> int

val ( lor ): int -> int -> int

val ( lxor ): int -> int -> int

val lnot: int -> int

val ( lsl ): int -> int -> int

val ( lsr ): int -> int -> int

val ( asr ): int -> int -> int

(* Float arithmetic *)
val ( +. ): float -> float -> float

val ( -. ): float -> float -> float

val ( *. ): float -> float -> float

val ( /. ): float -> float -> float

val ( ~-. ): float -> float

val ( ~+. ): float -> float

val float: int -> float

val floor: float -> float

val ceil: float -> float

val sqrt: float -> float

val exp: float -> float

val log: float -> float

val log10: float -> float

val cos: float -> float

val sin: float -> float

val tan: float -> float

val acos: float -> float

val asin: float -> float

val atan: float -> float

val atan2: float -> float -> float

val cosh: float -> float

val sinh: float -> float

val tanh: float -> float

val acosh: float -> float

val asinh: float -> float

val atanh: float -> float

val expm1: float -> float

val log1p: float -> float

val copysign: float -> float -> float

val mod_float: float -> float -> float

val frexp: float -> float * int

val ldexp: float -> int -> float

val modf: float -> float * float

val float_of_int: int -> float

val int_of_float: float -> int

val truncate: float -> int

val string_of_int: int -> string

val string_of_float: float -> string

val int_of_string: string -> int

val int_of_string_opt: string -> int option

val float_of_string: string -> float

val float_of_string_opt: string -> float option

val string_of_bool: bool -> string

val bool_of_string: string -> bool

val bool_of_string_opt: string -> bool option

val ( ^ ): string -> string -> string

val ( @ ): 'a list -> 'a list -> 'a list

val infinity: float

val neg_infinity: float

val nan: float

val max_float: float

val min_float: float

val epsilon_float: float

(* Boolean operations *)
val not: bool -> bool

val ( && ): bool -> bool -> bool

val ( || ): bool -> bool -> bool

(* Utility functions *)
val ignore: 'a -> unit

val ( |> ): 'a -> ('a -> 'b) -> 'b

val ( @@ ): ('a -> 'b) -> 'a -> 'b

(** Panic with a message and backtrace *)
val fst: 'a * 'b -> 'a

val snd: 'a * 'b -> 'b

val panic: string -> 'a

(* Array operations - flattened from Stdlib.Array *)
val array__get: 'a array -> int -> 'a

val array__set: 'a array -> int -> 'a -> unit

val array__make: int -> 'a -> 'a array

val array__init: int -> (int -> 'a) -> 'a array

val array__length: 'a array -> int

val array__unsafe_get: 'a array -> int -> 'a

val array__unsafe_set: 'a array -> int -> 'a -> unit

(* Sys operations - flattened from Stdlib.Sys *)
val sys__getenv: string -> string

exception Sys__Not_found

(* Unix operations - flattened from Unix *)
val unix__putenv: string -> string -> unit

val unix__environment: unit -> string array

val unix__getcwd: unit -> string

val unix__chdir: string -> unit

val domain__recommended_domain_count: unit -> int
