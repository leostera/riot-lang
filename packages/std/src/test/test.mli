type test_case

val case : string -> (unit -> (unit, string) result) -> test_case
val skip : string -> (unit -> (unit, string) result) -> test_case
val assert_equal : expected:'a -> actual:'a -> unit
val assert_ok : ('a, 'b) result -> unit
val assert_error : ('a, 'b) result -> unit
val assert_true : bool -> unit
val assert_false : bool -> unit

module Cli : sig
  val main :
    name:string ->
    tests:test_case list ->
    args:string list ->
    (unit, Miniriot.Process.exit_reason) result
end
