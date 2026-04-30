(**
   Function application inference.

   This module owns the argument-matching rules for calls. `Typer` still owns
   expression recursion and passes `infer_expression` in as a callback so the
   large mutually-recursive inference chain does not leak into this module.
*)

(**
   Infer a function application.

   The returned type is the type produced after applying every supplied
   argument. Labeled arguments are matched by label rather than only by
   position, and omitted optional parameters may be skipped when a later
   positional argument is supplied.
*)
val infer:
  State.t ->
  infer_expression:(Ast.expression -> Ast.Type.t) ->
  Ast.application ->
  Ast.Type.t
