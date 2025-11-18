(* Property-based tests for Datalog engine *)
open Std
open Propane
open Datalog

(* ============================================================================
   Phase 0: Query-Only Datalog
   
   Note: Tests for rule evaluation and Universe.InMemory have been disabled
   since Phase 0 focuses on query-only functionality.
   
   Relation property tests remain active.
   ============================================================================ *)

(* Helper: Compare two relations for equality *)
let relation_equal r1 r2 =
  let l1 = Relation.to_list r1 in
  let l2 = Relation.to_list r2 in
  List.length l1 = List.length l2 &&
  List.for_all2 (fun t1 t2 ->
    List.length t1 = List.length t2 &&
    List.for_all2 Value.equal t1 t2
  ) l1 l2

(* Custom arbitrary for Value.t *)
let value_arb =
  Arbitrary.make (
    Generator.one_of [
      Generator.map (fun i -> Value.Int i) 
        (Generator.int_range (-100) 100);
      Generator.map (fun s -> Value.String s) 
        (Generator.string_size (Generator.int_range 0 10) Generator.char_lowercase);
    ]
  )

(* Custom arbitrary for fact tuples (list of values) *)
let tuple_arb =
  Arbitrary.make (
    Generator.list_size 
      (Generator.int_range 1 5)
      value_arb.gen
  )

(* Custom arbitrary for relations (list of tuples) *)
let relation_tuples_arb =
  Arbitrary.make (
    Generator.list_size
      (Generator.int_range 0 20)
      tuple_arb.gen
  )

(* Property 1: Relation merge is commutative *)
let prop_merge_commutative = 
  property "relation merge is commutative"
    Arbitrary.(pair relation_tuples_arb relation_tuples_arb)
    (fun (t1, t2) ->
      let r1 = Relation.of_list t1 in
      let r2 = Relation.of_list t2 in
      let merged_12 = Relation.merge r1 r2 in
      let merged_21 = Relation.merge r2 r1 in
      relation_equal merged_12 merged_21)

(* Property 2: Relation merge is associative *)
let prop_merge_associative =
  property "relation merge is associative"
    Arbitrary.(triple relation_tuples_arb relation_tuples_arb relation_tuples_arb)
    (fun (t1, t2, t3) ->
      let r1 = Relation.of_list t1 in
      let r2 = Relation.of_list t2 in
      let r3 = Relation.of_list t3 in
      
      (* (r1 ∪ r2) ∪ r3 *)
      let left = Relation.merge (Relation.merge r1 r2) r3 in
      
      (* r1 ∪ (r2 ∪ r3) *)
      let right = Relation.merge r1 (Relation.merge r2 r3) in
      
      relation_equal left right)

(* Property 3: Merge with empty relation is identity *)
let prop_merge_identity =
  property "merge with empty relation is identity"
    relation_tuples_arb
    (fun tuples ->
      let r = Relation.of_list tuples in
      let empty = Relation.of_list [] in
      
      (* r ∪ ∅ = r *)
      let merged_right = Relation.merge r empty in
      (* ∅ ∪ r = r *)
      let merged_left = Relation.merge empty r in
      
      relation_equal r merged_right && relation_equal r merged_left)

(* Property 4: Relation is always sorted *)
let prop_relation_sorted =
  property "relation is always sorted"
    relation_tuples_arb
    (fun tuples ->
      let r = Relation.of_list tuples in
      let lst = Relation.to_list r in
      
      (* Check if list is sorted according to tuple comparison *)
      let rec is_sorted = function
        | [] | [_] -> true
        | t1 :: t2 :: rest ->
            compare t1 t2 <= 0 && is_sorted (t2 :: rest)
      in
      is_sorted lst)

(* Property 5: Deduplication is idempotent *)
let prop_dedup_idempotent =
  property "relation dedup is idempotent"
    relation_tuples_arb
    (fun tuples ->
      (* First dedup via of_list *)
      let r1 = Relation.of_list tuples in
      let deduped = Relation.to_list r1 in
      
      (* Second dedup *)
      let r2 = Relation.of_list deduped in
      
      (* Should be identical *)
      relation_equal r1 r2)

(* Property 6: Relation length is at most input length (due to dedup) *)
let prop_length_bounded =
  property "relation length ≤ input length (deduplication)"
    relation_tuples_arb
    (fun tuples ->
      let r = Relation.of_list tuples in
      Relation.length r <= List.length tuples)

(* Property 7: Empty relation has length 0 *)
let prop_empty_length =
  property "empty relation has length 0"
    (Arbitrary.make (Generator.return ()))
    (fun () ->
      let empty = Relation.of_list [] in
      Relation.length empty = 0)

let tests = [
  prop_merge_commutative;
  prop_merge_associative;
  prop_merge_identity;
  prop_relation_sorted;
  prop_dedup_idempotent;
  prop_length_bounded;
  prop_empty_length;
]
