open Global

(** The type of test: regular unit test or property test with example count. *)
type test_type =
  | UnitTest
  | Property of {
      examples : int;
    }
(** [case name fn] creates a regular unit test. *)
type test_case
val case : string -> (unit -> (unit, string) result) -> test_case

(** [property name ~examples fn] creates a property test.
    Use this for property-based tests to show the number of examples tested.
    
    Example:
    {[
      Test.property "my property" ~examples:1000 (fun () ->
        (* property checking logic *)
        Ok ()
      )
    ]}
*)
val property : string -> examples:int -> (unit -> (unit, string) result) -> test_case

(** [skip name fn] creates a skipped test. *)
val skip : string -> (unit -> (unit, string) result) -> test_case

val todo : string -> test_case

(** [todo name] creates a placeholder test marked as todo. *)
val assert_equal : expected:'a -> actual:'a -> unit

val assert_error : ('a, 'b) result -> unit

val assert_false : bool -> unit

val assert_ok : ('a, 'b) result -> unit

val assert_true : bool -> unit

module Cli : sig
  val main : name:string ->
  tests:test_case list ->
  args:string list ->
  (unit, Miniriot.Process.exit_reason) result
end
