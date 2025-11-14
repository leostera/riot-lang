open Std

(** {1 Join - Efficient Relation Joins}
    
    Join operations are the heart of Datalog evaluation. When we have a rule like:
    
    {[
      path(X, Z) :- edge(X, Y), path(Y, Z)
    ]}
    
    We need to join the [edge] relation with the [path] relation on the shared
    variable [Y].
    
    {2 Performance}
    
    Since our relations are sorted, we can use merge join algorithms which are
    O(n + m) instead of O(n * m) for naive nested loops.
    
    {2 Example}
    
    {[
      (* Facts *)
      edge: [(1, 2); (2, 3); (3, 4)]
      path: [(1, 2); (1, 3); (2, 3); (2, 4)]
      
      (* Rule: path(X, Z) :- edge(X, Y), path(Y, Z) *)
      (* Join on Y: edge.to = path.from *)
      
      (* Results: *)
      (1, 3)  (* edge(1,2) + path(2,3) *)
      (1, 4)  (* edge(1,2) + path(2,4) *)
      (2, 4)  (* edge(2,3) + path(3,4) *)
    ]}
*)

(** {2 Basic Join} *)

type join_result = {
  substitution : Substitution.t;
  tuple : Storage.fact_tuple;
}
(** Result of joining: variable bindings + the resulting tuple *)

val join_atoms : 
  Ast.atom -> Storage.fact_tuple Relation.t ->
  Ast.atom -> Storage.fact_tuple Relation.t ->
  join_result list
(** Join two relations on their shared variables.
    
    Given two atoms (patterns) and their corresponding relations (facts),
    find all combinations where the shared variables can be unified.
    
    Example:
    {[
      let atom1 = atom ~predicate:"edge" ~args:[Var "X"; Var "Y"] in
      let rel1 = Relation.of_list [[Int 1; Int 2]; [Int 2; Int 3]] in
      
      let atom2 = atom ~predicate:"path" ~args:[Var "Y"; Var "Z"] in
      let rel2 = Relation.of_list [[Int 2; Int 3]; [Int 2; Int 4]] in
      
      let results = join_atoms atom1 rel1 atom2 rel2 in
      (* Returns combinations where Y matches:
         - edge(1, 2) + path(2, 3) → {X→1, Y→2, Z→3}, tuple:[1,2,3]
         - edge(1, 2) + path(2, 4) → {X→1, Y→2, Z→4}, tuple:[1,2,4]
         - edge(2, 3) + path(3, ?) → none (no path from 3)
      *)
    ]}
*)

(** {2 Cartesian Product} *)

val cartesian_product : 
  Storage.fact_tuple Relation.t ->
  Storage.fact_tuple Relation.t ->
  (Storage.fact_tuple * Storage.fact_tuple) list
(** Compute cartesian product of two relations.
    For testing and as a fallback when no shared variables exist.
    
    Warning: O(n * m) complexity! Use only for small relations or testing.
*)

(** {2 Projection} *)

val project : vars:string list -> Substitution.t -> Storage.fact_tuple option
(** Project substitution onto specific variables to create a tuple.
    Variables must be in order and all must be bound.
    
    Example:
    {[
      let sub = Substitution.of_list [
        ("X", Int 1); ("Y", Int 2); ("Z", Int 3)
      ] in
      
      project ~vars:["X"; "Z"] sub
      (* Some [Int 1; Int 3] *)
      
      project ~vars:["X"; "W"] sub
      (* None - W not bound *)
    ]}
*)

(** {2 Utilities} *)

val shared_vars : Ast.atom -> Ast.atom -> string list
(** Find variables that appear in both atoms.
    
    Example:
    {[
      let atom1 = atom ~predicate:"edge" ~args:[Var "X"; Var "Y"] in
      let atom2 = atom ~predicate:"path" ~args:[Var "Y"; Var "Z"] in
      
      shared_vars atom1 atom2  (* ["Y"] *)
    ]}
*)

val atom_vars : Ast.atom -> string list
(** Extract all variable names from an atom (deduplicated).
    
    Example:
    {[
      let atom = atom ~predicate:"foo" 
        ~args:[Var "X"; Var "Y"; Var "X"] in
      
      atom_vars atom  (* ["X"; "Y"] *)
    ]}
*)
