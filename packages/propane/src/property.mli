open Std

(** Property module for defining and checking properties.
    
    Properties are universally quantified statements about values.
    This module provides the core property testing functionality.
*)
(** {1 Core Types} *)

type property_result =
  | Success
  | Failure of { counter_example: string; shrink_steps: int }
  | Error of { exception_: exn; backtrace: string }
  | Assumption_violated
(** Result of running a property test. *)
type test_property

(** An opaque property that can be tested. *)
(** {1 Property Configuration} *)

type config = {
  test_count: int;
  max_shrink_steps: int;
  seed: int option;
  verbose: bool;
}

(** Configuration for property testing.
    - [test_count]: Number of test cases to generate (default: 100)
    - [max_shrink_steps]: Maximum shrinking iterations (default: 1000)
    - [seed]: Optional fixed seed for reproducibility
    - [verbose]: Print verbose output (default: false) *)
val default_config: config

(** Default configuration with sensible defaults. *)
(** {1 Creating Properties} *)

val property: string -> 'value Arbitrary.t -> ('value -> bool) -> Test.test_case

(** [property name arb predicate] creates a Std.Test.test_case directly.
    
    Example:
    {[
      let list_rev_prop = 
        property "list reverse is involutive" 
          Arbitrary.(list int) 
          (fun lst -> List.rev (List.rev lst) = lst)
      
      let tests = [ list_rev_prop ]
    ]} *)
val for_all: 'value Arbitrary.t -> ('value -> bool) -> test_property

(** [for_all arb predicate] creates an internal property (for advanced use).
    Prefer {!property} for normal usage. *)
(** {1 Assumptions} *)

val implies: bool -> bool -> bool

(** [implies precondition conclusion] checks logical implication.
    If [precondition] is false, the test case is skipped. *)
val assume: bool -> unit

(** [assume cond] skips the current test case if [cond] is false.
    
    Example:
    {[
      property "division property"
        Arbitrary.(pair int int)
        (fun (a, b) ->
          assume (b <> 0);
          (a / b) * b + (a mod b) = a)
    ]} *)
val assume_fail: unit -> 'value

(** [assume_fail ()] unconditionally fails the assumption.
    Useful in pattern matching to skip certain branches. *)
(** {1 Failure Reporting} *)

val fail: string -> 'value

(** [fail msg] fails the property test with a custom message. *)
(** {1 Running Properties} *)

val check: ?config:config -> test_property -> property_result

(** [check ~config prop] runs the property test and returns the result.
    This NEVER throws exceptions - all failures are returned as results. *)
(** {1 Internal API} *)

val get_name: test_property -> string

(** Get the name of a property. Used by Test module. *)
