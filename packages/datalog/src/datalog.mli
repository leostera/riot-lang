open Std

(** {1 Datalog - High-Performance Datalog Engine}
    
    A Datalog engine for OCaml inspired by Datafrog, Crepe, and DataScript.
    
    {2 Quick Start}
    
    {[
      open Datalog
      
      let u = create () in
      let u = add_fact u "edge(1, 2)" |> Result.unwrap in
      let u = add_fact u "edge(2, 3)" |> Result.unwrap in
      let u = add_rule u "path(X,Y) :- edge(X,Y)" |> Result.unwrap in
      let u = add_rule u "path(X,Z) :- edge(X,Y), path(Y,Z)" |> Result.unwrap in
      
      match query u "path(X, Y)" with
      | Ok results -> (* Process bindings *)
      | Error e -> eprintln "Query failed: %s" e
    ]}
*)

(** {2 Core Modules} *)

module Parser = Parser
(** Parser for Datalog syntax *)

module Ast = Ast
(** Abstract syntax tree types *)

module Ast_from_cst = Ast_from_cst
(** Convert parser CST to AST *)

module Term = Term
(** Datalog terms: variables, constants, wildcards *)

module Value = Value
(** Concrete values: integers, strings, URIs *)

module Relation = Relation
(** Sorted tuple storage *)

module Storage = Storage
(** Pluggable storage interface *)

module InmemoryStorage = Inmemory_storage
(** Default in-memory storage backend *)

module Universe = Universe
(** Datalog knowledge base (facts + rules + storage) *)

module Substitution = Substitution
(** Variable-to-value bindings *)

module Unify = Unify
(** Pattern matching and unification *)

module Join = Join
(** Efficient relation joins *)

module Variable = Variable
(** Semi-naive evaluation support *)

module Evaluator = Evaluator
(** Fixed-point rule evaluation *)

(** {2 Main Types} 

    Note: Evaluation engine complete! 🎉
*)

(** {2 Status}
    
    - ✅ Parser complete (150 tests passing)
    - 🔨 AST types complete (Week 1, Day 1-2)
    - 🔨 Relation storage complete (Week 1, Day 3-4)
    - ⏳ Universe and evaluation engine (Week 1-2)
    - ⏳ Full query API (Week 3)
*)
