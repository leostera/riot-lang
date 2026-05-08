(**
   Sequential Operation Property Tests for SwissTable

   Tests random sequences of operations (insert, get, remove, clear)
   to verify that Swisstable maintains correctness under arbitrary
   operation orderings. This catches state management bugs.
*)
open Std
open Propane

(** {1 Operation Types} *)

let bounded_list_arb = fun ?(min = 0) max elem_arb ->
  let list_arb = Arbitrary.list elem_arb in
  { list_arb with gen = Generator.list_size (Generator.int_range min max) elem_arb.gen }

let non_empty_bounded_list_arb = fun max elem_arb -> bounded_list_arb ~min:1 max elem_arb

type operation =
  | Insert of int * int
  | Get of int
  | Remove of int
  | Clear
  | ContainsKey of int
  | Len
  | IsEmpty

(** {1 Operation Generators} *)

(* Generate a random operation *)

let operation_gen =
  Generator.frequency
    [
      (
        40,
        Generator.map
          (fun (k, v) -> Insert (k, v))
          (Generator.pair (Generator.int_range 0 50) Arbitrary.int.gen)
      );
      (30, Generator.map (fun k -> Get k) (Generator.int_range 0 50));
      (20, Generator.map (fun k -> Remove k) (Generator.int_range 0 50));
      (5, Generator.return Clear);
      (3, Generator.map (fun k -> ContainsKey k) (Generator.int_range 0 50));
      (1, Generator.return Len);
      (1, Generator.return IsEmpty);
    ]

let operation_arb =
  Arbitrary.make
    ~print:(fun __tmp1 ->
      match __tmp1 with
      | Insert (k, v) -> "Insert(" ^ Int.to_string k ^ ", " ^ Int.to_string v ^ ")"
      | Get k -> "Get(" ^ Int.to_string k ^ ")"
      | Remove k -> "Remove(" ^ Int.to_string k ^ ")"
      | Clear -> "Clear"
      | ContainsKey k -> "ContainsKey(" ^ Int.to_string k ^ ")"
      | Len -> "Len"
      | IsEmpty -> "IsEmpty")
    operation_gen

(** {1 Operation Application} *)

(* Apply an operation to both Swisstable and HashMap, verify they match *)

let apply_operation = fun op swiss hash ->
  match op with
  | Insert (k, v) ->
      let r1 = Swisstable.insert swiss k v in
      let r2 = Collections.HashMap.insert hash ~key:k ~value:v in
      if not (r1 = r2) then
        fail ("Insert(" ^ Int.to_string k ^ "," ^ Int.to_string v ^ "): results differ")
  | Get k ->
      let r1 = Swisstable.get swiss k in
      let r2 = Collections.HashMap.get hash ~key:k in
      if not (r1 = r2) then
        fail ("Get(" ^ Int.to_string k ^ "): results differ")
  | Remove k ->
      let r1 = Swisstable.remove swiss k in
      let r2 = Collections.HashMap.remove hash ~key:k in
      if not (r1 = r2) then
        fail ("Remove(" ^ Int.to_string k ^ "): results differ")
  | Clear ->
      Swisstable.clear swiss;
      Collections.HashMap.clear hash;
      if not (Swisstable.len swiss = 0) || not (Collections.HashMap.length hash = 0) then
        fail "Clear: maps not empty after clear"
  | ContainsKey k ->
      let r1 = Swisstable.contains_key swiss k in
      let r2 = Collections.HashMap.has_key hash ~key:k in
      if not (r1 = r2) then
        fail ("ContainsKey(" ^ Int.to_string k ^ "): results differ")
  | Len ->
      let l1 = Swisstable.len swiss in
      let l2 = Collections.HashMap.length hash in
      if not (l1 = l2) then
        fail ("Len: lengths differ (swiss=" ^ Int.to_string l1 ^ ", hash=" ^ Int.to_string l2 ^ ")")
  | IsEmpty ->
      let e1 = Swisstable.is_empty swiss in
      let e2 = Collections.HashMap.is_empty hash in
      if not (e1 = e2) then
        fail "IsEmpty: results differ"

(** {1 Sequential Properties} *)

(* Property 1: Random operation sequence maintains equivalence *)

let random_sequence_prop =
  property
    "random operation sequence: maintains equivalence with hashmap"
    (non_empty_bounded_list_arb 100 operation_arb)
    (fun ops ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Apply all operations *)
      List.for_each ops ~fn:(fun op -> apply_operation op swiss hash);
      (* Final state verification *)
      if not (Swisstable.len swiss = Collections.HashMap.length hash) then
        fail "Final lengths differ";
      let swiss_list = Swisstable.to_list swiss in
      List.for_all (fun (k, v) -> Collections.HashMap.get hash ~key:k = Some v) swiss_list)

(* Property 2: Insert-heavy sequence *)

let insert_heavy_sequence_prop =
  property
    "insert-heavy sequence: correctness maintained"
    (non_empty_bounded_list_arb 100 Arbitrary.(pair int int))
    (fun pairs ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Insert all *)
      List.for_each pairs ~fn:(fun (k, v) -> apply_operation (Insert (k, v)) swiss hash);
      (* Get all *)
      List.for_each pairs ~fn:(fun (k, _) -> apply_operation (Get k) swiss hash);
      true)

(* Property 3: Remove-heavy sequence *)

let remove_heavy_sequence_prop =
  property
    "remove-heavy sequence: correctness maintained"
    (non_empty_bounded_list_arb 100 Arbitrary.(pair int int))
    (fun pairs ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Insert all *)
      List.for_each pairs ~fn:(fun (k, v) -> apply_operation (Insert (k, v)) swiss hash);
      (* Remove all *)
      List.for_each pairs ~fn:(fun (k, _) -> apply_operation (Remove k) swiss hash);
      (* Both should be empty *)
      Swisstable.is_empty swiss && Collections.HashMap.is_empty hash)

(* Property 4: Interleaved inserts and removes *)

let interleaved_ops_prop =
  property
    "interleaved insert/remove: correctness maintained"
    (non_empty_bounded_list_arb 50 Arbitrary.int)
    (fun keys ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Insert, then remove, then insert again *)
      List.for_each
        keys
        ~fn:(fun k ->
          apply_operation (Insert (k, k * 2)) swiss hash;
          apply_operation (Remove k) swiss hash;
          apply_operation (Insert (k, k * 3)) swiss hash);
      (* Verify final values *)
      List.for_all
        (fun k ->
          Swisstable.get swiss k = Some (k * 3)
          && Collections.HashMap.get hash ~key:k = Some (k * 3))
        keys)

(* Property 5: Clear in the middle of operations *)

let clear_interleaved_prop =
  property
    "clear in middle: correctness maintained"
    (Arbitrary.pair
      (bounded_list_arb 50 Arbitrary.(pair int int))
      (bounded_list_arb 50 Arbitrary.(pair int int)))
    (fun (before_clear, after_clear) ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Insert before clear *)
      List.for_each before_clear ~fn:(fun (k, v) -> apply_operation (Insert (k, v)) swiss hash);
      (* Clear *)
      apply_operation Clear swiss hash;
      (* Insert after clear *)
      List.for_each after_clear ~fn:(fun (k, v) -> apply_operation (Insert (k, v)) swiss hash);
      (* Verify only after_clear entries present *)
      Swisstable.len swiss = Collections.HashMap.length hash && List.for_all
        (fun (k, _) ->
          let in_swiss = Swisstable.contains_key swiss k in
          let in_hash = Collections.HashMap.has_key hash ~key:k in
          in_swiss = in_hash)
        before_clear)

(* Property 6: Contains checks interspersed *)

let contains_checks_prop =
  property
    "contains_key checks: always match hashmap"
    (bounded_list_arb 50 Arbitrary.(pair int int))
    (fun pairs ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Insert with contains_key checks after each *)
      List.for_all
        (fun (k, v) ->
          apply_operation (Insert (k, v)) swiss hash;
          let c1 = Swisstable.contains_key swiss k in
          let c2 = Collections.HashMap.has_key hash ~key:k in
          if not c1 || not c2 || not (c1 = c2) then
            fail ("Contains check failed after insert(" ^ Int.to_string k ^ ")");
          true)
        pairs)

(* Property 7: Length checks after each operation *)

let length_invariant_prop =
  property
    "length invariant: maintained after each operation"
    (bounded_list_arb 100 operation_arb)
    (fun ops ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Apply each operation and check length matches *)
      List.for_all
        (fun op ->
          apply_operation op swiss hash;
          let l1 = Swisstable.len swiss in
          let l2 = Collections.HashMap.length hash in
          if not (l1 = l2) then
            fail
              (
                "Length mismatch after " ^ (
                  match operation_arb.print with
                  | Some p -> p op
                  | None -> "operation"
                )
              );
          true)
        ops)

(* Property 8: to_list consistency throughout *)

let to_list_invariant_prop =
  property
    "to_list: entries always accessible"
    (bounded_list_arb 50 Arbitrary.(pair int int))
    (fun pairs ->
      let swiss = Swisstable.create () in
      (* Insert all *)
      List.for_each
        pairs
        ~fn:(fun (k, v) ->
          let _ = Swisstable.insert swiss k v in
          ());
      (* All entries in to_list should be gettable *)
      let entries = Swisstable.to_list swiss in
      List.for_all
        (fun (k, v) ->
          match Swisstable.get swiss k with
          | Some v' when v = v' -> true
          | _ ->
              fail ("to_list entry (" ^ Int.to_string k ^ "," ^ Int.to_string v ^ ") not gettable"))
        entries)

(* Property 9: Overwrite sequence *)

let overwrite_sequence_prop =
  property
    "overwrite sequence: latest value wins"
    (Arbitrary.pair Arbitrary.int (non_empty_bounded_list_arb 20 Arbitrary.int))
    (fun (key, values) ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Insert same key with different values *)
      List.for_each values ~fn:(fun v -> apply_operation (Insert (key, v)) swiss hash);
      (* Latest value should be present *)
      let latest = List.get_unchecked values ~at:(List.length values - 1) in
      Swisstable.get swiss key = Some latest && Collections.HashMap.get hash ~key:key = Some latest)

(* Property 10: Empty checks throughout *)

let empty_invariant_prop =
  property
    "is_empty: consistent with len = 0"
    (bounded_list_arb 50 operation_arb)
    (fun ops ->
      let swiss = Swisstable.create () in
      (* Apply each operation and verify is_empty consistency *)
      List.for_all
        (fun op ->
          match op with
          | Insert (k, v) ->
              Swisstable.insert swiss k v
              |> ignore;
              true
          | Remove k ->
              Swisstable.remove swiss k
              |> ignore;
              true
          | Clear ->
              Swisstable.clear swiss;
              true
          | _ ->
              (* Check is_empty matches len = 0 *)
              let empty = Swisstable.is_empty swiss in
              let zero_len = Swisstable.len swiss = 0 in
              if not (empty = zero_len) then
                fail "is_empty inconsistent with len";
              true)
        ops)

(** {1 Stress Test - Many Sequential Operations} *)

(* Property 11: Long operation sequence *)

let long_sequence_prop =
  property
    "long sequence (200 ops): correctness maintained"
    Arbitrary.int
    (fun _seed ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Fixed sequence of operations *)
      for i = 0 to 199 do
        let op =
          match i mod 5 with
          | 0 -> Insert (i mod 30, i)
          | 1 -> Get (i mod 30)
          | 2 -> Remove (i mod 30)
          | 3 -> ContainsKey (i mod 30)
          | _ -> Len
        in
        apply_operation op swiss hash
      done;
      (* Final verification *)
      Swisstable.len swiss = Collections.HashMap.length hash)

(** {1 Test Suite} *)

let tests = [
  random_sequence_prop;
  insert_heavy_sequence_prop;
  remove_heavy_sequence_prop;
  interleaved_ops_prop;
  clear_interleaved_prop;
  contains_checks_prop;
  length_invariant_prop;
  to_list_invariant_prop;
  overwrite_sequence_prop;
  empty_invariant_prop;
  long_sequence_prop;
]

let main ~args = Test.Cli.main ~name:"swisstable-sequential-tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
