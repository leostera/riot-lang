(* Property-based tests for Datalog engine *)
open Std
open Propane
open Datalog

(* ============================================================================
   Phase 1: Relation Properties
   
   These test fundamental algebraic properties of the Relation data structure:
   - Commutativity (a ∪ b = b ∪ a)
   - Associativity ((a ∪ b) ∪ c = a ∪ (b ∪ c))
   - Identity (a ∪ ∅ = a)
   - Sorted invariant (result is always sorted)
   - Idempotence (dedup twice = dedup once)
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

(* ============================================================================
   Phase 2: Evaluator Properties
   
   These test the MOST CRITICAL properties of the Datalog evaluation engine:
   - Idempotence: eval(eval(U)) = eval(U)  (Fixed-point!)
   - Monotonicity: facts1 ⊆ facts2 ⟹ results1 ⊆ results2
   - Determinism: Same input always produces same output
   ============================================================================ *)

module Eval = Evaluator.Make(Universe.InMemory)

(* Custom generators for Datalog types *)

(* Generate a valid predicate name: lowercase starting, alphanumeric *)
let predicate_gen =
  Generator.map
    (fun s -> 
      if String.length s = 0 then "p" 
      else String.lowercase_ascii (String.sub s 0 1) ^ 
           (if String.length s > 1 then String.sub s 1 (String.length s - 1) else ""))
    (Generator.string_size (Generator.int_range 1 5) Generator.char_lowercase)

(* Generate a valid variable name: uppercase starting *)
let variable_gen =
  Generator.map
    (fun c -> String.make 1 (Char.uppercase_ascii c))
    Generator.char_lowercase

(* Generator for Term.t *)
let term_gen =
  Generator.one_of [
    Generator.return Term.Wildcard;
    Generator.map (fun v -> Term.Const v) value_arb.gen;
    Generator.map (fun v -> Term.Var v) variable_gen;
  ]

(* Generator for ground atoms (facts - no variables) *)
let fact_gen =
  Generator.map2
    (fun predicate values ->
      let args = List.map (fun v -> Term.Const v) values in
      Ast.atom ~predicate ~args)
    predicate_gen
    (Generator.list_size (Generator.int_range 1 3) value_arb.gen)

(* Generator for atoms (may have variables) *)
let atom_gen =
  Generator.map2
    (fun predicate terms ->
      Ast.atom ~predicate ~args:terms)
    predicate_gen
    (Generator.list_size (Generator.int_range 1 3) term_gen)

(* Generator for simple rules (single body atom, no negation/builtins) *)
let simple_rule_gen =
  Generator.map2
    (fun head body_atom ->
      Ast.rule ~head ~body:[Ast.Atom body_atom])
    atom_gen
    atom_gen

(* Generator for small programs (facts only, for now) *)
let simple_program_gen =
  Generator.map
    (fun facts -> Ast.program ~facts ~rules:[])
    (Generator.list_size (Generator.int_range 0 10) fact_gen)

(* Generator for programs with simple rules *)
let program_with_rules_gen =
  Generator.map2
    (fun facts rules -> Ast.program ~facts ~rules)
    (Generator.list_size (Generator.int_range 1 10) fact_gen)
    (Generator.list_size (Generator.int_range 0 2) simple_rule_gen)

(* Helper: Build universe from program *)
let build_universe (program : Ast.program) =
  (* Group facts by predicate *)
  let facts_by_pred = ref [] in
  List.iter (fun (fact : Ast.atom) ->
    let tuple = List.map (function
      | Term.Const v -> v
      | _ -> Value.Int 0  (* Shouldn't happen for facts *)
    ) fact.args in
    
    match List.assoc_opt fact.predicate !facts_by_pred with
    | Some existing ->
        facts_by_pred := (fact.predicate, tuple :: existing) ::
          (List.remove_assoc fact.predicate !facts_by_pred)
    | None ->
        facts_by_pred := (fact.predicate, [tuple]) :: !facts_by_pred
  ) program.Ast.facts;
  
  let universe = Universe.InMemory.of_facts !facts_by_pred in
  
  (* Add all rules *)
  List.fold_left (fun u rule ->
    Universe.InMemory.add_rule u rule
  ) universe program.Ast.rules

(* Helper: Get all facts from a universe *)
let get_all_facts universe =
  let predicates = Universe.InMemory.predicates universe in
  List.concat_map (fun pred ->
    let rel = Universe.InMemory.get_facts universe ~predicate:pred in
    let tuples = Relation.to_list rel in
    List.map (fun tuple -> (pred, tuple)) tuples
  ) predicates

(* Helper: Check if fact set1 is subset of set2 *)
let facts_subset facts1 facts2 =
  List.for_all (fun f1 -> List.mem f1 facts2) facts1

(* Property 8: Evaluation is idempotent (CRITICAL - proves fixed-point!) *)
let prop_eval_idempotent =
  property "evaluation is idempotent (fixed-point)"
    (Arbitrary.make simple_program_gen)
    (fun program ->
      let u1 = build_universe program in
      let evaluated1 = Eval.eval u1 in
      let facts1 = get_all_facts evaluated1 in
      
      (* Evaluate again *)
      let evaluated2 = Eval.eval evaluated1 in
      let facts2 = get_all_facts evaluated2 in
      
      (* Should be identical - fixed point! *)
      List.length facts1 = List.length facts2 &&
      facts_subset facts1 facts2 &&
      facts_subset facts2 facts1)

(* Property 9: Evaluation is monotonic (CRITICAL - Datalog guarantee!) *)
let prop_eval_monotonic =
  property "evaluation is monotonic (adding facts preserves results)"
    Arbitrary.(pair (make simple_program_gen) (make fact_gen))
    (fun (program, extra_fact) ->
      (* Evaluate original program *)
      let u1 = build_universe program in
      let evaluated1 = Eval.eval u1 in
      let facts1 = get_all_facts evaluated1 in
      
      (* Add extra fact and evaluate *)
      let program2 = { program with Ast.facts = extra_fact :: program.Ast.facts } in
      let u2 = build_universe program2 in
      let evaluated2 = Eval.eval u2 in
      let facts2 = get_all_facts evaluated2 in
      
      (* All original facts should still be present *)
      facts_subset facts1 facts2)

(* Property 10: Evaluation is deterministic (CRITICAL - same input = same output) *)
let prop_eval_deterministic =
  property "evaluation is deterministic"
    (Arbitrary.make simple_program_gen)
    (fun program ->
      (* Evaluate twice from scratch *)
      let u1 = build_universe program in
      let evaluated1 = Eval.eval u1 in
      let facts1 = get_all_facts evaluated1 in
      
      let u2 = build_universe program in
      let evaluated2 = Eval.eval u2 in
      let facts2 = get_all_facts evaluated2 in
      
      (* Should produce identical results *)
      List.length facts1 = List.length facts2 &&
      facts_subset facts1 facts2 &&
      facts_subset facts2 facts1)

(* ============================================================================
   Phase 3: Unification & Join Properties
   
   These test pattern matching and relational join operations:
   - Unification: reflexivity, symmetry, wildcard behavior
   - Grounding: applying substitutions produces constants
   - Joins: size properties, empty join behavior
   - Substitutions: lookup/bind consistency
   ============================================================================ *)

(* Property 11: Unification is reflexive (term unifies with itself) *)
let prop_unify_reflexive =
  property "unify term with itself always succeeds"
    (Arbitrary.make term_gen)
    (fun term ->
      let sub = Substitution.empty () in
      match Unify.unify_terms sub term term with
      | Some _ -> true
      | None -> false)

(* Property 12: Unification is symmetric *)
let prop_unify_symmetric =
  property "unification is symmetric"
    Arbitrary.(pair (make term_gen) (make term_gen))
    (fun (t1, t2) ->
      let sub = Substitution.empty () in
      let unified1 = Unify.unify_terms sub t1 t2 in
      let unified2 = Unify.unify_terms sub t2 t1 in
      match unified1, unified2 with
      | Some _, Some _ -> true  (* Both succeed *)
      | None, None -> true      (* Both fail *)
      | _ -> false)             (* Asymmetric - shouldn't happen! *)

(* Property 13: Wildcard always unifies *)
let prop_wildcard_unifies =
  property "wildcard unifies with anything"
    (Arbitrary.make term_gen)
    (fun term ->
      let sub = Substitution.empty () in
      match Unify.unify_terms sub Term.Wildcard term with
      | Some _ -> true
      | None -> false)

(* Property 14: Grounding a variable with its binding gives the value *)
let prop_ground_binding =
  property "grounding variable with binding gives value"
    Arbitrary.(pair (make variable_gen) value_arb)
    (fun (var, value) ->
      let sub = Substitution.of_list [(var, value)] in
      let term = Term.Var var in
      match Unify.ground sub term with
      | Some v -> Value.equal v value
      | None -> false)

(* Property 15: Cartesian product size is product of input sizes *)
let prop_cartesian_size =
  property "cartesian product size = r1.size * r2.size"
    Arbitrary.(pair relation_tuples_arb relation_tuples_arb)
    (fun (t1, t2) ->
      let r1 = Relation.of_list t1 in
      let r2 = Relation.of_list t2 in
      let product = Join.cartesian_product r1 r2 in
      List.length product = Relation.length r1 * Relation.length r2)

(* Property 16: Join with empty relation gives empty *)
let prop_join_empty =
  property "join with empty relation gives empty"
    (Arbitrary.make fact_gen)
    (fun fact ->
      let rel = Relation.of_list [[Value.Int 1; Value.Int 2]] in
      let empty = Relation.of_list [] in
      
      (* Create two atoms that should join *)
      let atom1 = fact in
      let atom2 = fact in
      
      let results = Join.join_atoms atom1 rel atom2 empty in
      List.length results = 0)

(* Property 17: Substitution lookup returns bound value *)
let prop_substitution_lookup =
  property "lookup returns bound value"
    Arbitrary.(pair (make variable_gen) value_arb)
    (fun (var, value) ->
      let sub = Substitution.empty () in
      let sub' = Substitution.bind sub ~var ~value in
      match Substitution.lookup sub' ~var with
      | Some v -> Value.equal v value
      | None -> false)

(* Property 18: Empty substitution has no bindings *)
let prop_empty_substitution =
  property "empty substitution has size 0"
    (Arbitrary.make (Generator.return ()))
    (fun () ->
      let sub = Substitution.empty () in
      Substitution.size sub = 0)

(* All tests *)
let tests = [
  (* Phase 1: Relation properties *)
  prop_merge_commutative;
  prop_merge_associative;
  prop_merge_identity;
  prop_relation_sorted;
  prop_dedup_idempotent;
  prop_length_bounded;
  prop_empty_length;
  
  (* Phase 2: Evaluator properties (CRITICAL!) *)
  prop_eval_idempotent;
  prop_eval_monotonic;
  prop_eval_deterministic;
  
  (* Phase 3: Unification & Join properties *)
  prop_unify_reflexive;
  prop_unify_symmetric;
  prop_wildcard_unifies;
  prop_ground_binding;
  prop_cartesian_size;
  prop_join_empty;
  prop_substitution_lookup;
  prop_empty_substitution;
]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
      Test.Cli.main ~name:"datalog:property_tests" ~tests ~args:Env.args)
    ~args:Env.args ()
