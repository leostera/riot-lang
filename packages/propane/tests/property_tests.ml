(** Meta-tests: Using Propane to test Propane! *)
open Std
open Propane

(* Property: int_range always produces values in range *)

let int_range_property =
  property "int_range produces values in range" Arbitrary.(pair int int)
    (fun ((low, high)) ->
      (* Use reasonable bounds *)
      let low = low mod 100 in
      let high = high mod 100 in
      let (low, high) =
        if low <= high then
          (low, high)
        else
          (high, low)
      in
      (* Generate a value and check it's in range *)
      let gen = Generator.int_range low high in
      let rnd = Random.State.make [|42|] in
      let value = Generator.generate rnd gen in
      value >= low && value <= high)

(* Property: list reverse is involutive (meta-test to verify property checking works) *)

let list_reverse_property =
  property
    "list reverse is involutive"
    Arbitrary.(list int)
    (fun lst -> List.rev (List.rev lst) = lst)

(* Property: list length is preserved by reverse *)

let list_length_property =
  property
    "list length preserved by reverse"
    Arbitrary.(list int)
    (fun lst -> List.length (List.rev lst) = List.length lst)

(* Property: map distributes over list append *)

let map_append_property =
  property "map distributes over append" Arbitrary.(triple (list int) (list int) int)
    (fun ((xs, ys, n)) ->
      let f x = x + n in
      let mapped_concat = List.map f (xs @ ys) in
      let concat_mapped = List.map f xs @ List.map f ys in
      mapped_concat = concat_mapped)

(* Property: string concatenation is associative *)

let string_concat_property =
  property
    "string concatenation is associative"
    Arbitrary.(triple string string string)
    (fun ((a, b, c)) -> (a ^ b) ^ c = a ^ (b ^ c))

(* Property: string length is additive *)

let string_length_property =
  property
    "string length is additive"
    Arbitrary.(pair string string)
    (fun ((s1, s2)) -> String.length (s1 ^ s2) = String.length s1 + String.length s2)

(* Property: bool negation is involutive *)

let bool_negation_property =
  property "bool negation is involutive" Arbitrary.bool (fun b -> not (not b) = b)

(* Property: pair construction and destruction *)

let pair_property =
  property "pair fst/snd round-trip" Arbitrary.(pair int string)
    (fun p ->
      let (a, b) = p in
      fst p = a && snd p = b)

(* Property: option map preserves None *)

let option_none_property =
  property "option map preserves None" Arbitrary.(option int)
    (fun opt ->
      let f x = x * 2 in
      match opt with
      | None -> Option.map f opt = None
      | Some _ -> true)

(* Property: int addition is commutative *)

let int_commutative_property =
  property "int addition is commutative" Arbitrary.(pair int int)
    (fun ((a, b)) ->
      (* Use small-ish values to avoid overflow *)
      let a = a mod 10_000 in
      let b = b mod 10_000 in
      a + b = b + a)

(* Property: int addition is associative *)

let int_associative_property =
  property "int addition is associative" Arbitrary.(triple int int int)
    (fun ((a, b, c)) ->
      (* Use small-ish values to avoid overflow *)
      let a = a mod 10_000 in
      let b = b mod 10_000 in
      let c = c mod 10_000 in
      (a + b) + c = a + (b + c))

(* Property: assumes work correctly *)

let assume_property =
  property "assumes filter test cases" Arbitrary.int
    (fun n ->
      assume (n >= 0);
      n >= 0)

let tests = [
  int_range_property;
  list_reverse_property;
  list_length_property;
  map_append_property;
  string_concat_property;
  string_length_property;
  bool_negation_property;
  pair_property;
  option_none_property;
  int_commutative_property;
  int_associative_property;
  assume_property;
]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"propane/property_tests" ~tests ~args)
    ~args:Env.args
    ()
