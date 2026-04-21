(** Examples demonstrating shrinking and finding minimal counter-examples *)
open Std
open Propane

(* This property will FAIL intentionally to demonstrate shrinking *)

let failing_list_prop =
  property "DEMO: list sum is always positive (FALSE - will find counter-example)" Arbitrary.(list int)
    (fun lst ->
      let sum =
        List.fold_left lst ~init:0 ~fn:(fun acc value -> acc + value)
      in
      sum >= 0)

(* This is false when list contains negative numbers *)

(* This will fail and shrink to minimal counter-example *)

let failing_string_length_prop =
  property
    "DEMO: string length never exceeds 5 (FALSE - demonstrates shrinking)"
    Arbitrary.string
    (fun s -> String.length s <= 5)

(* This demonstrates shrinking finding the boundary *)

let failing_boundary_prop =
  property "DEMO: all ints are less than 50 (FALSE)" Arbitrary.int (fun n -> n < 50)

(* Custom shrinker example *)

let custom_shrink_demo_prop =
  let my_int_arb = Arbitrary.make ~shrink:(Shrinker.towards 100) ~print:Printer.int Generator.int in
  property "DEMO: all ints equal 100 (FALSE - shrinks towards 100)" my_int_arb (fun n -> n = 100)

(* Demonstrating shrinking with complex types *)

let failing_nested_prop =
  property "DEMO: nested lists never contain empty sublist (FALSE)" Arbitrary.(list (list int))
    (fun nested ->
      let has_empty =
        List.any nested ~fn:(fun sublist -> List.length sublist = 0)
      in
      not has_empty)

(* Example showing minimal shrinking *)

let minimal_failure_prop =
  property
    "DEMO: pairs where both are positive (FALSE)"
    Arbitrary.(pair int int)
    (fun ((a, b)) -> a > 0 && b > 0)

(* Will shrink to find minimal negative case *)

(* ========================================== *)

(* PASSING properties that demonstrate good shrinking behavior *)

(* ========================================== *)

(* This passes, showing shrinking helps debug *)

let passing_with_assume_prop =
  property "PASS: division by non-zero works" Arbitrary.(pair int int)
    (fun ((a, b)) ->
      assume (b != 0);
      let result = a / b in
      result * b + (a mod b) = a)

(* Property that would fail without assumptions *)

let sqrt_with_assume_prop =
  property "PASS: sqrt with proper domain" Arbitrary.float
    (fun x ->
      assume (x >= 0.0);
      Float.sqrt x >= 0.0)

(*
  To run and see shrinking in action:
  
  1. Run this file - the DEMO properties will fail
  2. Observe the counter-examples found
  3. Notice how Propane shrinks them to minimal cases
  
  Expected outputs:
  - failing_list_prop: shrinks to something like `[-1]`
  - failing_string_length_prop: shrinks to a 6-character string
  - failing_boundary_prop: shrinks to `50` or nearby
  - custom_shrink_demo_prop: shrinks toward `100`
  - failing_nested_prop: shrinks to `[[]]`
  - minimal_failure_prop: shrinks to `(-1, 0)` or `(0, -1)` or similar
*)

let tests = [ passing_with_assume_prop; sqrt_with_assume_prop; ]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"propane-shrinking-examples" ~tests ~args ())
    ~args:Env.args
    ()
