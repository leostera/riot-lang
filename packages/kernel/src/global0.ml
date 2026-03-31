(** Core primitives from Stdlib that don't depend on any other modules *)
(* Re-export basic types from Types *)

include Types

include Ops

let compare = Stdlib.compare

let min = Stdlib.min

let max = Stdlib.max

(* Exception handling *)

exception Exit = Stdlib.Exit

let raise = Stdlib.raise

let raise_notrace = Stdlib.raise_notrace

let exit = Stdlib.exit

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
let panic = fun msg ->
    let exception Panic of string in
    Stdlib.raise (Panic msg)

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
