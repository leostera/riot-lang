open Std

(** Define and run universally-quantified tests.

    Use this module when an example-based test is too narrow and you want to
    check that a rule holds across many generated inputs.

    Example:
    ```ocaml
    let reverse_is_involutive =
      Property.property
        "list reverse is involutive"
        Arbitrary.(list int)
        (fun xs -> List.rev (List.rev xs) = xs)
    ```
*)

type property_result =
  | (** The property held for all generated inputs. *)
    Success
  | (** The property failed and Propane found a counter-example.

         [counter_example] is the rendered failing input, and [shrink_steps]
         tells you how many shrinking passes were needed to minimize it.

         Example:
         ```ocaml
         Failure {
           counter_example = "[0; 0; 0; 0]";
           shrink_steps = 3;
         }
         ```
     *)
    Failure of {
      counter_example: string;
      shrink_steps: int;
    }
  | (** Running the property raised an exception.

         Use this to distinguish "the property returned false" from
         "the property code crashed".
     *)
    Error of {
      exception_: exn;
      backtrace: string;
    }
  | (** The generated input was discarded because an assumption did not hold. *)
    Assumption_violated

(** Result of running a property. *)

type test_property

(** Opaque property value used by the lower-level execution API. *)

type config = {
  (** Number of random inputs to try before reporting success. *)
  test_count: int;
  (** Maximum number of shrinking passes after a failure is found. *)
  max_shrink_steps: int;
  (** Optional deterministic seed for reproducible runs. *)
  seed: int option;
  (** Whether to print verbose progress while checking the property. *)
  verbose: bool;
}

(** Runtime configuration for property checking. *)

(** Default property-checking configuration.

    The default configuration runs a moderate number of tests, shrinks failing
    cases aggressively, and leaves the random seed unset.
*)
val default_config: config

(** Build a [Std.Test.test_case] from a property.

    This is the main entry point for integrating Propane with [riot test] and
    [Std.Test].

    Example:
    ```ocaml
    let tests =
      [
        Property.property
          "append preserves length"
          Arbitrary.(pair (list int) (list int))
          (fun (left, right) ->
            List.length (left @ right) = List.length left + List.length right);
      ]
    ```
*)
val property:
  (** Human-readable test name shown by the test runner. *)
  string ->
  (** Arbitrary used to generate and shrink inputs. *)
  'value Arbitrary.t ->
  (** Predicate that should hold for every generated input. *)
  ('value -> bool) ->
  Test.test_case

(** Build a property value without wrapping it into [Std.Test].

    Use this when you need to call {!check} directly or build your own
    execution flow around properties.
*)
val for_all: 'value Arbitrary.t -> ('value -> bool) -> test_property

(** Logical implication for conditional properties.

    [implies precondition conclusion] returns [true] whenever the precondition
    is false, and otherwise returns [conclusion].

    Example:
    ```ocaml
    Property.implies (b != 0) (((a / b) * b) + (a mod b) = a)
    ```
*)
val implies: bool -> bool -> bool

(** Abort the current generated case when a precondition does not hold.

    Use [assume] for properties that only make sense for a subset of the input
    domain, such as non-empty lists or non-zero divisors.
*)
val assume: bool -> unit

(** Abort the current generated case immediately.

    This is useful in pattern matches where only some branches are valid.
*)
val assume_fail: unit -> 'value

(** Fail the current property run with a custom explanation.

    Use this when returning [false] would hide the reason the property failed.
*)
val fail: string -> 'value

(** Run a property and collect the result without raising exceptions.

    Example return values:
    - [Success] when every generated input satisfies the property.
    - [Failure _] when a counter-example is found.
    - [Error _] when the property function raises.
*)
val check:
  (** Optional runtime configuration for the property run. *)
  ?config:config ->
  test_property ->
  property_result

(** Return the display name of a property.

    This is mainly useful when integrating Propane with other runners or
    reporting layers.
*)
val get_name: test_property -> string
