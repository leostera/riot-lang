type ('value, 'error) result =
  | Ok of 'value
  | Error of 'error
external raise: exn -> 'a = "%raise"

val ( = ): 'value -> 'value -> bool

val compare: 'value -> 'value -> int

val min: 'value -> 'value -> 'value

val max: 'value -> 'value -> 'value

val ( != ): 'value -> 'value -> bool

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

val ( @@ ): ('value -> 'result) -> 'value -> 'result

val ( |> ): 'value -> ('value -> 'result) -> 'result

val ( ^ ): string -> string -> string

val ( @ ): 'value list -> 'value list -> 'value list

val ( ** ): float -> float -> float

val not: bool -> bool

val ( && ): bool -> bool -> bool

val ( || ): bool -> bool -> bool
