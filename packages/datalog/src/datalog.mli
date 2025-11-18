open Std

(** {1 Datalog - Query-Only Datalog Engine}
    
    Phase 0: Query-Only Datalog Core
    
    A thin, streaming query layer over storage backends (like Poneglyph).
    
    {2 Quick Start}
    
    {[
      open Datalog
      
      (* Create universe with Poneglyph storage at snapshot *)
      module U = Universe.Make(PoneglyphStorage)
      let storage = Poneglyph.open_db "db" in
      
      (* Single-goal query *)
      let pattern = Ast.atom ~predicate:"language" 
        ~args:[Var "F"; Const (String "ocaml")] in
      
      module Eval = Evaluator.Make(U) in
      let results = Eval.query universe pattern in
      
      (* Stream results - first in <100ms! *)
      Iter.MutIterator.iter (fun sub ->
        println (Substitution.to_string sub)
      ) results
      
      (* Multi-goal query *)
      let clauses = [
        Ast.Atom (Ast.atom ~predicate:"language" ~args:[Var "F"; Const (String "ocaml")]);
        Ast.Atom (Ast.atom ~predicate:"size" ~args:[Var "F"; Var "S"]);
      ] in
      let results = Eval.multi_query universe clauses in
      (* Streaming join - no materialization! *)
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
(** Lazy iterator-based relations *)

module Storage = Storage
(** Pluggable storage interface *)

module Universe = Universe
(** Datalog snapshot view over storage *)

module Substitution = Substitution
(** Variable-to-value bindings *)

module Unify = Unify
(** Pattern matching and unification *)

module Join = Join
(** Join support for multi-goal queries *)

module Variable = Variable
(** Variable tracking *)

module Evaluator = Evaluator
(** Query-only evaluation (no rules) *)
