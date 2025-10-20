open Std

type t = Base64 | QuotedPrintable | SevenBit | EightBit | Binary

val of_string : string -> (t, string) Result.t
val to_string : t -> string
val decode : t -> string -> (string, string) Result.t
val encode : t -> string -> (string, string) Result.t
