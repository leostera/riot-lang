external equal : 'a -> 'a -> bool = "%equal"

let ( = ) = equal

external not_equal : 'a -> 'a -> bool = "%notequal"

let ( != ) = not_equal

external less_than : 'a -> 'a -> bool = "%lessthan"

let ( < ) = less_than

external greater_than : 'a -> 'a -> bool = "%greaterthan"

let ( > ) = greater_than

external less_or_equal : 'a -> 'a -> bool = "%lessequal"

let ( <= ) = less_or_equal

external greater_or_equal : 'a -> 'a -> bool = "%greaterequal"

let ( >= ) = greater_or_equal

external neg_int : int -> int = "%negint"

let ( ~- ) = neg_int

external add_int : int -> int -> int = "%addint"

let ( + ) = add_int

external sub_int : int -> int -> int = "%subint"

let ( - ) = sub_int

external mul_int : int -> int -> int = "%mulint"

let ( * ) = mul_int

external div_int : int -> int -> int = "%divint"

let ( / ) = div_int

external rem_int : int -> int -> int = "%modint"

let ( mod ) = rem_int

external int_logand : int -> int -> int = "%andint"

let ( land ) = int_logand

external int_logor : int -> int -> int = "%orint"

let ( lor ) = int_logor

external int_logxor : int -> int -> int = "%xorint"

let ( lxor ) = int_logxor

let lnot value = value lxor (-1)

external shift_left_int : int -> int -> int = "%lslint"

let ( lsl ) = shift_left_int

external shift_right_logical_int : int -> int -> int = "%lsrint"

let ( lsr ) = shift_right_logical_int

external shift_right_int : int -> int -> int = "%asrint"

let ( asr ) = shift_right_int

external not : bool -> bool = "%boolnot"

external and_bool : bool -> bool -> bool = "%sequand"

let ( && ) = and_bool

external or_bool : bool -> bool -> bool = "%sequor"

let ( || ) = or_bool
