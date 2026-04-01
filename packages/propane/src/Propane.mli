(** {0 Propane: Property-Based Testing for Riot}
    
    Propane is a property-based testing library inspired by PropEr (Erlang).
    It provides powerful generators, intelligent shrinking, and seamless
    integration with Std.Test.
    
    {1 Philosophy}
    
    Instead of writing individual test cases, you write {i properties} that
    should hold for all inputs. Propane generates hundreds of random test cases,
    and if a property fails, automatically finds the minimal counter-example
    through {i shrinking}.
    
    {1 Quick Start}
    
    {[
      open Std
      open Propane
      
      (* Property: reversing a list twice gives the original *)
      let list_rev_prop = 
        property "list reverse is involutive" 
          Arbitrary.(list int) 
          (fun lst -> List.rev (List.rev lst) = lst)
      
      (* Property with assumptions *)
      let division_prop =
        property "division and modulo relation"
          Arbitrary.(pair int int)
          (fun (a, b) ->
            assume (b != 0);  (* Only test when b is non-zero *)
            (a / b) * b + (a mod b) = a)
      
      (* Run tests *)
      let tests = [ list_rev_prop; division_prop ]
      
      let () =
        Miniriot.run 
          ~main:(fun ~args -> Test.Cli.main ~name:"my-tests" ~tests ~args) 
          ~args:Env.args ()
    ]}
    
    {1 Core Concepts}
    
    {2 Generators}
    
    Generators produce random values. Propane provides generators for all
    standard types:
    
    {[
      Generator.int                    (* Random integers *)
      Generator.string                 (* Random strings *)
      Generator.list Generator.int     (* Random lists of ints *)
      Generator.pair gen1 gen2         (* Random pairs *)
    ]}
    
    Combine generators to create complex data:
    
    {[
      (* Generate random points *)
      let point_gen =
        Generator.map
          (fun (x, y) -> { x; y })
          (Generator.pair (Generator.int_range 0 100) 
                          (Generator.int_range 0 100))
    ]}
    
    {2 Arbitraries}
    
    Arbitraries combine generators with shrinkers and printers:
    
    {[
      Arbitrary.int           (* Has generator, shrinker, and printer *)
      Arbitrary.string        (* Strings with shrinking support *)
      Arbitrary.list arb      (* Lists with element shrinking *)
      Arbitrary.option arb    (* Optional values *)
    ]}
    
    Create custom arbitraries:
    
    {[
      type color = Red | Green | Blue
      
      let color_arb = Arbitrary.make
        ~print:(function
          | Red -> "Red" | Green -> "Green" | Blue -> "Blue")
        Generator.(one_of [return Red; return Green; return Blue])
    ]}
    
    {2 Properties}
    
    Properties are assertions that should hold for all inputs:
    
    {[
      (* Basic property *)
      property "addition is commutative"
        Arbitrary.(pair int int)
        (fun (a, b) -> a + b = b + a)
      
      (* Property with assumptions *)
      property "sqrt works for positive numbers"
        Arbitrary.float
        (fun x ->
          assume (x >= 0.0);
          Float.sqrt x >= 0.0)
      
      (* Property with explicit failure *)
      property "validate input"
        Arbitrary.string
        (fun s ->
          if String.length s > 1000 then
            fail "String too long!"
          else
            true)
    ]}
    
    {2 Shrinking}
    
    When a property fails, Propane automatically finds the minimal
    counter-example:
    
    {v
    Property "all lists are short" failed:
    Counter-example (after 5 shrink steps):
      [0; 0; 0; 0; 0; 0]
    v}
    
    Custom shrinkers can be defined:
    
    {[
      let point_shrinker point =
        let x_shrunk = Shrinker.shrink (Shrinker.towards 0) point.x in
        let y_shrunk = Shrinker.shrink (Shrinker.towards 0) point.y in
        List.map (fun x -> { x; y = point.y }) x_shrunk @
        List.map (fun y -> { x = point.x; y }) y_shrunk
    ]}
    
    {1 Examples}
    
    {2 Testing Collection Properties}
    
    {[
      (* Vector operations *)
      property "vector push/pop round-trip"
        Arbitrary.(pair int (vector int))
        (fun (x, vec) ->
          Collections.Vector.push vec x;
          match Collections.Vector.pop vec with
          | Some y -> x = y
          | None -> false)
      
      (* HashMap operations *)
      property "hashmap get after insert"
        Arbitrary.(triple string int (hashmap string int))
        (fun (key, value, map) ->
          Collections.HashMap.insert map key value |> ignore;
          Collections.HashMap.get map key = Some value)
    ]}
    
    {2 Testing String Operations}
    
    {[
      property "string concatenation is associative"
        Arbitrary.(triple string string string)
        (fun (a, b, c) -> (a ^ b) ^ c = a ^ (b ^ c))
      
      property "string length is additive"
        Arbitrary.(pair string string)
        (fun (s1, s2) ->
          String.length (s1 ^ s2) = String.length s1 + String.length s2)
    ]}
    
    {2 Custom Generators}
    
    {[
      (* Generate valid email addresses *)
      let email_gen =
        Generator.map
          (fun (name, domain) -> name ^ "@" ^ domain ^ ".com")
          (Generator.pair 
            (Generator.string_of Generator.char_lowercase)
            (Generator.string_of Generator.char_lowercase))
      
      (* Use frequency for weighted generation *)
      let mostly_small =
        Generator.frequency [
          (9, Generator.int_range 0 10);      (* 90% small *)
          (1, Generator.int_range 100 1000);  (* 10% large *)
        ]
    ]}
    
    {1 Advanced Usage}
    
    {2 Assumptions and Preconditions}
    
    Use [assume] to filter invalid test cases:
    
    {[
      property "division properties"
        Arbitrary.(pair int int)
        (fun (a, b) ->
          assume (b != 0);           (* Skip when b = 0 *)
          assume (a mod b = 0);      (* Only divisible pairs *)
          (a / b) * b = a)
    ]}
    
    Use [implies] for conditional properties:
    
    {[
      implies (b != 0) ((a / b) * b + (a mod b) = a)
    ]}
    
    {2 Configuration}
    
    Control test execution:
    
    {[
      let config = Property.{
        test_count = 1000;        (* Run 1000 tests *)
        max_shrink_steps = 500;   (* Max shrinking iterations *)
        seed = Some 42;           (* Reproducible tests *)
        verbose = true;           (* Print progress *)
      }
      
      Property.check ~config my_property
    ]}
    
    {1 Best Practices}
    
    - Start with simple properties and build up complexity
    - Use assumptions sparingly - too many can slow down testing
    - Write properties that are obviously true (sanity checks)
    - Test invariants, commutativity, associativity, identity laws
    - Combine multiple properties to thoroughly test a function
    - Use custom generators for domain-specific data
    - Add shrinking and printing for better error messages
    
    {1 See Also}
    
    - {!Generator} - Creating random test data
    - {!Shrinker} - Minimizing counter-examples
    - {!Arbitrary} - Combining generators with shrinking and printing
    - {!Property} - Defining and checking properties
    - {!Printer} - Pretty-printing values
*)

module Generator = Generator

(** Random value generation. See {!Generator}. *)
module Shrinker = Shrinker

(** Counter-example minimization. See {!Shrinker}. *)
module Printer = Printer

(** Value pretty-printing. See {!Printer}. *)
module Arbitrary = Arbitrary

(** Arbitraries combine generators, shrinkers, and printers. See {!Arbitrary}. *)
module Property = Property

(** Property definition and checking. See {!Property}. *)
(** {1 Convenience API}
    
    These functions are re-exported from their respective modules for convenience. *)

(** {2 Property Creation} *)

val property: string -> 'value Arbitrary.t -> ('value -> bool) -> Std.Test.test_case

(** [property name arb predicate] creates a property test that can be run
    with Std.Test.
    
    This is the main way to define properties. It returns a [Test.test_case]
    that can be added to your test suite.
    
    {[
      let my_prop = property "list reverse is involutive"
        Arbitrary.(list int)
        (fun lst -> List.rev (List.rev lst) = lst)
      
      let tests = [ my_prop ]
    ]}
    
    The property will:
    - Generate 100 random test cases (configurable)
    - Run the predicate on each
    - If a failure is found, shrink to find minimal counter-example
    - Return Pass or Fail with error message
*)
val for_all: 'value Arbitrary.t -> ('value -> bool) -> Property.test_property

(** [for_all arb predicate] creates a property without a name.
    
    Useful for composing properties or running manually:
    
    {[
      let prop = for_all Arbitrary.int (fun n -> n + 0 = n)
      Property.check prop  (* Run the property *)
    ]}
*)
(** {2 Assumptions} *)

val implies: bool -> bool -> bool

(** [implies precondition conclusion] expresses a conditional property.
    
    If the precondition is false, the test case is discarded. If true,
    the conclusion must hold.
    
    {[
      property "division works"
        Arbitrary.(pair int int)
        (fun (a, b) ->
          implies (b != 0) ((a / b) * b + (a mod b) = a))
    ]}
    
    Equivalent to:
    {[
      if not precondition then true else conclusion
    ]}
*)
val assume: bool -> unit

(** [assume condition] filters test cases where the condition is false.
    
    If the condition is false, the current test case is discarded and
    a new one is generated. Use this to express preconditions.
    
    {[
      property "sqrt squared"
        Arbitrary.float
        (fun x ->
          assume (x >= 0.0);  (* Only test non-negative *)
          let sqrt_x = Float.sqrt x in
          sqrt_x *. sqrt_x >= x -. 0.0001)
    ]}
    
    {b Warning:} Too many failing assumptions can slow down testing
    or cause tests to fail with "too many assumptions violated".
*)
val assume_fail: unit -> 'value

(** [assume_fail ()] unconditionally fails the current assumption.
    
    Equivalent to [assume false] but clearer in intent:
    
    {[
      property "valid dates only"
        Arbitrary.(pair int int)
        (fun (month, day) ->
          if month < 1 || month > 12 then assume_fail ();
          if day < 1 || day > 31 then assume_fail ();
          (* ... rest of property ... *)
        )
    ]}
*)
(** {2 Explicit Failures} *)

val fail: string -> 'value

(** [fail message] explicitly fails a property with a custom error message.
    
    Useful for providing context about why a property failed:
    
    {[
      property "validate input format"
        Arbitrary.string
        (fun s ->
          if String.length s = 0 then
            fail "Empty string not allowed"
          else if String.length s > 1000 then
            fail "String too long (max 1000 chars)"
          else
            validate_format s)
    ]}
    
    The message will be included in the test failure output.
*)
