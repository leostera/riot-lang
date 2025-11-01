(** Common types re-exported from Stdlib for use in nostdlib packages *)

type ('value, 'error) result = ('value, 'error) Result.t =
  | Ok of 'value
  | Error of 'error

type 'a option = 'a Option.t = None | Some of 'a

(* Format types needed for Format.formatter *)
type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'd) Stdlib.format4
type ('a, 'b, 'c, 'd, 'e, 'f) format6 = ('a, 'b, 'c, 'd, 'e, 'f) Stdlib.format6

(** Format string helper *)
let format = Printf.sprintf

(** Async-safe write to stdout using Fs.File.write which handles Would_block *)
let write_stdout str =
  let bytes = Bytes.of_string str in
  let len = String.length str in
  let rec write_all offset remaining =
    if remaining = 0 then ()
    else
      match Fs.File.write IO.stdout ~pos:offset ~len:remaining bytes with
      | Ok n -> write_all (offset + n) (remaining - n)
      | Error _ -> ()  (* Silently ignore errors to prevent crashes *)
  in
  write_all 0 len

(** Async-safe print to stdout - never raises Sys_blocked_io *)
let print fmt = Printf.ksprintf write_stdout fmt

(** Async-safe print to stdout with newline - never raises Sys_blocked_io *)
let println fmt = Printf.ksprintf (fun s -> write_stdout (s ^ "\n")) fmt

(** Panic with a message and backtrace *)
let panic msg =
  let exception Panic of string in
  raise (Panic msg)

(* Comparison operators *)
let ( = ) = Stdlib.( = )
let ( <> ) = Stdlib.( <> )
let ( < ) = Stdlib.( < )
let ( > ) = Stdlib.( > )
let ( <= ) = Stdlib.( <= )
let ( >= ) = Stdlib.( >= )
let compare = Stdlib.compare
let min = Stdlib.min
let max = Stdlib.max

(* Exception handling *)
exception Exit = Stdlib.Exit

let raise = Stdlib.raise
let raise_notrace = Stdlib.raise_notrace
let failwith = Stdlib.failwith
let invalid_arg = Stdlib.invalid_arg
let exit = Stdlib.exit

(* Integer arithmetic *)
let ( + ) = Stdlib.( + )
let ( - ) = Stdlib.( - )
let ( * ) = Stdlib.( * )
let ( / ) = Stdlib.( / )
let ( mod ) = Stdlib.( mod )
let ( ~- ) = Stdlib.( ~- )
let ( ~+ ) = Stdlib.( ~+ )
let abs = Stdlib.abs
let succ = Stdlib.succ
let pred = Stdlib.pred
let max_int = Stdlib.max_int
let min_int = Stdlib.min_int

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
let ( ^ ) = Stdlib.( ^ )
let ( @ ) = Stdlib.( @ )
let infinity = Stdlib.infinity
let neg_infinity = Stdlib.neg_infinity
let nan = Stdlib.nan
let max_float = Stdlib.max_float
let min_float = Stdlib.min_float
let epsilon_float = Stdlib.epsilon_float

type fpclass = Stdlib.fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

let classify_float = Stdlib.classify_float

(* Boolean operations *)
let not = Stdlib.not
let ( && ) = Stdlib.( && )
let ( || ) = Stdlib.( || )

(* Utility functions *)
let ignore = Stdlib.ignore
let ( |> ) = Stdlib.( |> )
let ( @@ ) = Stdlib.( @@ )
let fst = Stdlib.fst
let snd = Stdlib.snd
let ( @ ) = Stdlib.( @ )
