(**
   Constraint application helpers for inference.

   `Unifier` reports structural errors. This module turns those errors into
   source-backed diagnostics at the call site that created each constraint.
*)

(** Apply a unification constraint and collect a diagnostic on failure. *)
val unify:
  State.t ->
  expected:Ast.Type.t ->
  actual:Ast.Type.t ->
  on_error:(Unifier.error -> Diagnostics.Diagnostic.t) ->
  unit

(** Diagnostic adapter for a source type annotation. *)
val annotation_diagnostic: Ast.core_type -> Unifier.error -> Diagnostics.Diagnostic.t

(** Diagnostic adapter for an expression type hint. *)
val expression_hint_diagnostic:
  Ast.expression ->
  Ast.expression_type_hint ->
  Unifier.error ->
  Diagnostics.Diagnostic.t

(** Diagnostic adapter for an expression-originated constraint. *)
val expression_constraint_diagnostic: Ast.expression -> Unifier.error -> Diagnostics.Diagnostic.t

(** Diagnostic adapter for an application argument constraint. *)
val argument_constraint_diagnostic: Ast.argument -> Unifier.error -> Diagnostics.Diagnostic.t

(** Diagnostic adapter for a pattern-originated constraint. *)
val pattern_constraint_diagnostic: Ast.pattern -> Unifier.error -> Diagnostics.Diagnostic.t
