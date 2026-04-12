type ('value, 'error) result =
  | Ok of 'value
  | Error of 'error

exception Invalid_argument of string

exception Failure of string

exception Not_found

external raise: exn -> 'a = "%raise"

external raise_notrace: exn -> 'a = "%raise_notrace"

external ignore: 'value -> unit = "%ignore"

val max_int: int

val min_int: int

val ( = ): 'value -> 'value -> bool

val compare: 'value -> 'value -> int

val min: 'value -> 'value -> 'value

val max: 'value -> 'value -> 'value

val ( != ): 'value -> 'value -> bool

val ( <> ): 'value -> 'value -> bool

val ( < ): 'value -> 'value -> bool

val ( > ): 'value -> 'value -> bool

val ( <= ): 'value -> 'value -> bool

val ( >= ): 'value -> 'value -> bool

val ( ~- ): int -> int

val ( + ): int -> int -> int

val ( - ): int -> int -> int

val ( * ): int -> int -> int

val ( / ): int -> int -> int

val ( mod ): int -> int -> int

val ( land ): int -> int -> int

val ( lor ): int -> int -> int

val ( lxor ): int -> int -> int

val lnot: int -> int

val ( lsl ): int -> int -> int

val ( lsr ): int -> int -> int

val ( asr ): int -> int -> int

val ( ~-. ): float -> float

val ( +. ): float -> float -> float

val ( -. ): float -> float -> float

val ( *. ): float -> float -> float

val ( /. ): float -> float -> float

val ( |> ): 'value -> ('value -> 'result) -> 'result

val ( ^ ): string -> string -> string

val ( ** ): float -> float -> float

val float_of_int: int -> float

val int_of_float: float -> int

val float: int -> float

val string_of_int: int -> string

val string_of_float: float -> string

val abs: int -> int

val mod_float: float -> float -> float

val sqrt: float -> float

val floor: float -> float

val ceil: float -> float

val not: bool -> bool

val ( && ): bool -> bool -> bool

val ( || ): bool -> bool -> bool
