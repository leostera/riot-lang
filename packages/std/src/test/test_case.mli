open Global

type test_result = Pass | Fail of string | Error of exn

type test_type = 
  | UnitTest
  | Property of { examples: int }
(** The type of test: regular unit test or property test with example count. *)

type t = { 
  name : string; 
  test_type : test_type;
  fn : unit -> (unit, string) result; 
  skip : bool 
}

val case : string -> (unit -> (unit, string) result) -> t
(** [case name fn] creates a regular unit test. *)

val property : string -> examples:int -> (unit -> (unit, string) result) -> t
(** [property name ~examples fn] creates a property test that ran [examples] test cases. *)

val skip : string -> (unit -> (unit, string) result) -> t
(** [skip name fn] creates a skipped test. *)

val todo : string -> t
(** [todo name] creates a placeholder test marked as todo. *)
