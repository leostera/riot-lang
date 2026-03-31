(* Concat operators *)

let ( ^ ) = Stdlib.( ^ )

let ( @ ) = Stdlib.( @ )

(* Comparison operators *)

let ( = ) = Stdlib.( = )

let ( != ) = Stdlib.( <> )

let ptr_eq = Stdlib.( == )

let ptr_not_eq = Stdlib.( != )

let ( < ) = Stdlib.( < )

let ( > ) = Stdlib.( > )

let ( <= ) = Stdlib.( <= )

let ( >= ) = Stdlib.( >= )

(* Integer arithmetic *)

let ( + ) = Stdlib.( + )

let ( - ) = Stdlib.( - )

let ( * ) = Stdlib.( * )

let ( ** ) = Stdlib.( ** )

let ( / ) = Stdlib.( / )

let ( mod ) = Stdlib.( mod )

let ( ~- ) = Stdlib.( ~- )

let ( ~+ ) = Stdlib.( ~+ )

let abs = Stdlib.abs

(* Bitwise operations *)

let ( land ) = Stdlib.( land )

let ( lor ) = Stdlib.( lor )

let ( lxor ) = Stdlib.( lxor )

let lnot = Stdlib.lnot

let ( lsl ) = Stdlib.( lsl )

let ( lsr ) = Stdlib.( lsr )

let ( asr ) = Stdlib.( asr )

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

(* Utility functions *)

let ( |> ) = Stdlib.( |> )

let ( @@ ) = Stdlib.( @@ )
