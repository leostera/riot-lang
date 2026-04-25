open Std

(**
   Define and run universally-quantified tests.

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
  | Success
  | Failure of { counter_example: string; shrink_steps: int }
  | Error of { exception_: exn; backtrace: string }
  | Assumption_violated

(** Result of running a property. *)
type test_property

(** Opaque property value used by the lower-level execution API. *)
type config = {
  (** Number of random inputs to try before reporting success. *)
  test_count: int;
  (** Maximum number of shrinking passes after a failure is found. *)
  max_shrink_steps: int;
  (** Largest size passed to sized generators during a property run. *)
  max_size: int;
  (** Optional deterministic seed for reproducible runs. *)
  seed: int option;
  (** Whether to print verbose progress while checking the property. *)
  verbose: bool;
}

(** Runtime configuration for property checking. *)
(**
   Default property-checking configuration.

   The default configuration runs a moderate number of tests, shrinks failing
   cases aggressively, and leaves the random seed unset.
*)
val default_config: config

(**
   Build a [Std.Test.test_case] from a property.

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
val property: string -> 'value Arbitrary.t -> ('value -> bool) -> Test.test_case

(**
   Build a property value without wrapping it into [Std.Test].

   Use this when you need to call {!check} directly or build your own
   execution flow around properties.
*)
val for_all: 'value Arbitrary.t -> ('value -> bool) -> test_property

(**
   Conditional implication with assumption semantics.

   [implies precondition conclusion] discards the current generated case when
   [precondition] is false. When [precondition] is true, it returns
   [conclusion].

   Example:
   ```ocaml
   Property.implies (b != 0) (((a / b) * b) + (a mod b) = a)
   ```
*)
val implies: bool -> bool -> bool

(**
   Abort the current generated case when a precondition does not hold.

   Use [assume] for properties that only make sense for a subset of the input
   domain, such as non-empty lists or non-zero divisors.
*)
val assume: bool -> unit

(**
   Abort the current generated case immediately.

   This is useful in pattern matches where only some branches are valid.
*)
val assume_fail: unit -> 'value

(**
   Fail the current property run with a custom explanation.

   Use this when returning [false] would hide the reason the property failed.
*)
val fail: string -> 'value

(**
   Run a property and collect the result without raising exceptions.

   Example return values:
   - [Success] when every generated input satisfies the property.
   - [Failure _] when a counter-example is found.
   - [Error _] when the property function raises.
*)
val check: ?config:config -> ?on_progress:(Test.Context.progress -> unit) -> test_property -> property_result

(**
   Return the display name of a property.

   This is mainly useful when integrating Propane with other runners or
   reporting layers.
*)
val get_name: test_property -> string
