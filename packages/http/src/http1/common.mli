(** Common types and utilities for HTTP/1.1 parsing *)
open Std

type 'a parse_result =
  | Done of { value: 'a; remaining: string }
  (** Successfully parsed + remaining input *)
  | Need_more
  (** Need more data to continue parsing *)
  | Error of string

(** Parse error with message *)
val find_substring: needle:string -> string -> int option

val split_at: string -> int -> string * string
