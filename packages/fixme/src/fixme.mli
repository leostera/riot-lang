(**
   Syntax-directed linting and autofix toolkit shared by `riot-fix` and
   package-provided rule runners.
*)

(** Rewrite operations and fix application helpers. *)
module Fix: module type of Fix

(** Typed rule identifiers shared across lint surfaces. *)
module Rule_id: module type of Rule_id

(** Long-form rule explanations. *)
module Explanation: module type of Explanation

(** Diagnostics produced by rules. *)
module Diagnostic: module type of Diagnostic

(** Rule definition and execution surface. *)
module Rule: module type of Rule

(** CST traversal helpers for writing rules. *)
module Traversal: module type of Traversal

(** Run rules against source text. *)
module Source_runner: module type of Source_runner

(** Helpers for rule-focused tests. *)
module Rule_test: module type of Rule_test
