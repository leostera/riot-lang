(**
   Mutable inference state.

   This owns allocation for solver variables and other per-run counters. It is
   query-local state for the one-shot checker, not a persisted module
   interface.
*)
module State: module type of State

(**
   Semantic lookup environment used by the inference state.

   `Env` keeps value scopes separate from module-level type, constructor, and
   nested-module tables. Unit tests cover this module directly because subtle
   scoping bugs here become hard-to-debug inference failures later.
*)
module Env: module type of Env

(**
   Type unification engine.

   The unifier works over `Ast.Type.t`, follows solver-variable links, and
   rejects impossible constraints such as infinite types.
*)
module Unifier: module type of Unifier

(** Reusable generalized type assigned to exported and local values. *)
module TypeScheme: module type of TypeScheme

(** Generalization and instantiation of type schemes. *)
module Quantifier: module type of Quantifier

(** Exported summary inferred for one source module. *)
module ModuleInterface: module type of ModuleInterface

(** Result of one inference/checking run. *)
type infer_result = {
  (** Exported module interface produced by checking the file. *)
  intf: ModuleInterface.t;
  (** Structured diagnostics collected during checking. *)
  diagnostics: Diagnostics.t;
}

(**
   Check a typed implementation in one shot.

   The returned AST annotations are written directly onto the input tree. The
   `ModuleInterface.t` is the exported summary that tests and future cache
   layers can render or persist.
*)
val check_implementation: Ast.implementation -> infer_result

(**
   Check a typed interface in one shot.

   This is currently a stub-shaped path while interface summaries are being
   built out, but it gives callers a typed entrypoint for `.mli` files.
*)
val check_interface: Ast.interface -> infer_result

(**
   Check a complete `Typ.Ast` in one shot.

   Dispatches to `check_implementation` or `check_interface` based on the file
   kind.
*)
val check: Ast.t -> infer_result
