(** Core primitives from Stdlib that don't depend on any other modules *)
(* Re-export basic types from Types *)

include Types

(* Comparison operators *)

external equal: 'a -> 'a -> bool = "%equal"

let ( = ) = equal

external not_equal: 'a -> 'a -> bool = "%notequal"

let ( != ) = not_equal

let ( <> ) = not_equal

external ptr_eq: 'a -> 'a -> bool = "%eq"

external ptr_not_eq: 'a -> 'a -> bool = "%noteq"

external less_than: 'a -> 'a -> bool = "%lessthan"

let ( < ) = less_than

external greater_than: 'a -> 'a -> bool = "%greaterthan"

let ( > ) = greater_than

external less_or_equal: 'a -> 'a -> bool = "%lessequal"

let ( <= ) = less_or_equal

external greater_or_equal: 'a -> 'a -> bool = "%greaterequal"

let ( >= ) = greater_or_equal

(* Integer arithmetic *)

external neg_int: int -> int = "%negint"

let ( ~- ) = neg_int

external id_int: int -> int = "%identity"

let ( ~+ ) = id_int

external add_int: int -> int -> int = "%addint"

let ( + ) = add_int

external sub_int: int -> int -> int = "%subint"

let ( - ) = sub_int

external mul_int: int -> int -> int = "%mulint"

let ( * ) = mul_int

external div_int: int -> int -> int = "%divint"

let ( / ) = div_int

external rem_int: int -> int -> int = "%modint"

let ( mod ) = rem_int

let abs value =
  if value >= 0 then
    value
  else
    -value

(* Bitwise operations *)

external int_logand: int -> int -> int = "%andint"

let ( land ) = int_logand

external int_logor: int -> int -> int = "%orint"

let ( lor ) = int_logor

external int_logxor: int -> int -> int = "%xorint"

let ( lxor ) = int_logxor

let lnot value = value lxor (-1)

external shift_left_int: int -> int -> int = "%lslint"

let ( lsl ) = shift_left_int

external shift_right_logical_int: int -> int -> int = "%lsrint"

let ( lsr ) = shift_right_logical_int

external shift_right_int: int -> int -> int = "%asrint"

let ( asr ) = shift_right_int

(* Float arithmetic *)

external neg_float: float -> float = "%negfloat"

let ( ~-. ) = neg_float

external id_float: float -> float = "%identity"

let ( ~+. ) = id_float

external add_float: float -> float -> float = "%addfloat"

let ( +. ) = add_float

external sub_float: float -> float -> float = "%subfloat"

let ( -. ) = sub_float

external mul_float: float -> float -> float = "%mulfloat"

let ( *. ) = mul_float

external div_float: float -> float -> float = "%divfloat"

let ( /. ) = div_float

external pow_float: float -> float -> float = "caml_power_float" "pow" [@@unboxed] [@@noalloc]

let ( ** ) = pow_float

(* Boolean operations *)

external not: bool -> bool = "%boolnot"

external and_bool: bool -> bool -> bool = "%sequand"

let ( && ) = and_bool

external or_bool: bool -> bool -> bool = "%sequor"

let ( || ) = or_bool

(* Utility functions *)

external revapply: 'a -> ('a -> 'b) -> 'b = "%revapply"

let ( |> ) = revapply

external apply: ('a -> 'b) -> 'a -> 'b = "%apply"

let ( @@ ) = apply

(* Concat operators *)

external string_length: string -> int = "%string_length"

external bytes_create: int -> bytes = "caml_create_bytes"

external string_blit: string -> int -> bytes -> int -> int -> unit = "caml_blit_string" [@@noalloc]

external bytes_unsafe_to_string: bytes -> string = "%bytes_to_string"

let ( ^ ) left right =
  let left_length = string_length left in
  let right_length = string_length right in
  let output = bytes_create (left_length + right_length) in
  string_blit left 0 output 0 left_length;
  string_blit right 0 output left_length right_length;
  bytes_unsafe_to_string output

let rec ( @ ) left right =
  match left with
  | [] -> right
  | head :: tail -> head :: (tail @ right)

let compare = Stdlib.compare

let min = Stdlib.min

let max = Stdlib.max

(* Exception handling *)

exception Exit = Stdlib.Exit

let raise = Stdlib.raise

let raise_notrace = Stdlib.raise_notrace

let exit: int -> 'a = Stdlib.exit

(* Integer arithmetic *)

let succ = Stdlib.succ

let pred = Stdlib.pred

let max_int = Stdlib.max_int

let min_int = Stdlib.min_int

let float = Stdlib.float

let floor = Stdlib.floor

let ceil = Stdlib.ceil

let sqrt = Stdlib.sqrt

let exp = Stdlib.exp

let log = Stdlib.log

let log10 = Stdlib.log10

let cos = Stdlib.cos

let sin = Stdlib.sin

let tan = Stdlib.tan

let acos = Stdlib.acos

let asin = Stdlib.asin

let atan = Stdlib.atan

let atan2 = Stdlib.atan2

let cosh = Stdlib.cosh

let sinh = Stdlib.sinh

let tanh = Stdlib.tanh

let acosh = Stdlib.acosh

let asinh = Stdlib.asinh

let atanh = Stdlib.atanh

let expm1 = Stdlib.expm1

let log1p = Stdlib.log1p

let copysign = Stdlib.copysign

let mod_float = Stdlib.mod_float

let frexp = Stdlib.frexp

let ldexp = Stdlib.ldexp

let modf = Stdlib.modf

let float_of_int = Stdlib.float_of_int

let int_of_float = Stdlib.int_of_float

let truncate = Stdlib.truncate

let string_of_int = Stdlib.string_of_int

let string_of_float = Stdlib.string_of_float

let int_of_string = Stdlib.int_of_string

let int_of_string_opt = Stdlib.int_of_string_opt

let float_of_string = Stdlib.float_of_string

let float_of_string_opt = Stdlib.float_of_string_opt

let string_of_bool = Stdlib.string_of_bool

let bool_of_string = Stdlib.bool_of_string

let bool_of_string_opt = Stdlib.bool_of_string_opt

let infinity = Stdlib.infinity

let neg_infinity = Stdlib.neg_infinity

let nan = Stdlib.nan

let max_float = Stdlib.max_float

let min_float = Stdlib.min_float

let epsilon_float = Stdlib.epsilon_float

(* Utility functions *)

let ignore = Stdlib.ignore

let fst = Stdlib.fst

let snd = Stdlib.snd

(** Panic with a message and backtrace *)
exception Panic of string

let panic = fun msg -> Stdlib.raise (Panic msg)

(* Array operations - flattened from Stdlib.Array *)

let array__get = Stdlib.Array.get

let array__set = Stdlib.Array.set

let array__make = Stdlib.Array.make

let array__init = Stdlib.Array.init

let array__length = Stdlib.Array.length

let array__unsafe_get = Stdlib.Array.unsafe_get

let array__unsafe_set = Stdlib.Array.unsafe_set

let array__blit = Stdlib.Array.blit

let array__copy = Stdlib.Array.copy

(* Sys operations - flattened from Stdlib.Sys *)

let sys__getenv = Stdlib.Sys.getenv

exception Sys__Not_found = Stdlib.Not_found

(* Unix operations - flattened from Unix *)

let unix__putenv = Unix.putenv

let unix__environment = Unix.environment

let unix__getcwd = Unix.getcwd

let unix__chdir = Unix.chdir

let domain__recommended_domain_count = Stdlib.Domain.recommended_domain_count

(* Uchar module *)

module Uchar = Stdlib.Uchar
