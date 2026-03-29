(* Comparison operators *)
val ( = ) : 'a -> 'a -> bool

val (!=) : 'a -> 'a -> bool

val ptr_eq : 'a -> 'a -> bool

val ptr_not_eq : 'a -> 'a -> bool

val ( < ) : 'a -> 'a -> bool

val ( > ) : 'a -> 'a -> bool

val ( <= ) : 'a -> 'a -> bool

val ( >= ) : 'a -> 'a -> bool

(* Integer arithmetic *)
val ( + ) : int -> int -> int

val ( - ) : int -> int -> int

val ( * ) : int -> int -> int

val ( ** ) : float -> float -> float

val ( / ) : int -> int -> int

val ( mod ) : int -> int -> int

val ( ~- ) : int -> int

val ( ~+ ) : int -> int

val abs : int -> int

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

val ( ^ ) : string -> string -> string

val ( @ ) : 'a list -> 'a list -> 'a list

(* Boolean operations *)
val not : bool -> bool

val ( && ) : bool -> bool -> bool

val ( || ) : bool -> bool -> bool

(* Utility functions *)
val ( |> ) : 'a -> ('a -> 'b) -> 'b

val ( @@ ) : ('a -> 'b) -> 'a -> 'b
