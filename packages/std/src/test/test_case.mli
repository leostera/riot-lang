type test_result = Pass | Fail of string | Error of exn
type t = { name : string; fn : unit -> (unit, string) result; skip : bool }

val case : string -> (unit -> (unit, string) result) -> t
val skip : string -> (unit -> (unit, string) result) -> t
val todo : string -> t
