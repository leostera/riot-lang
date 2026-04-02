open Std
open Propane

(* Simple property: reversing a list twice gives the original list *)

let list_rev_prop =
  property
    "list reverse is involutive"
    Arbitrary.(list int)
    (fun lst -> List.rev (List.rev lst) = lst)

(* Property with assumptions *)

let division_prop =
  property "division and modulo relation" Arbitrary.(pair int int)
    (fun ((a, b)) ->
      assume (b != 0);
      (a / b) * b + (a mod b) = a)

(* Vector property *)

let vector_length_prop =
  property "vector length after push" Arbitrary.(pair int (vector int))
    (fun ((x, vec)) ->
      let original_len = Collections.Vector.len vec in
      Collections.Vector.push vec x;
      Collections.Vector.len vec = original_len + 1)

(* String property *)

let string_concat_prop =
  property
    "string concatenation length"
    Arbitrary.(pair string string)
    (fun ((s1, s2)) -> String.length (s1 ^ s2) = String.length s1 + String.length s2)

(* All tests - ready to use with Test.Cli.main! *)

let tests = [ list_rev_prop; division_prop; vector_length_prop; string_concat_prop ]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"propane-basic-examples" ~tests ~args)
    ~args:Env.args
    ()
