open Std

(**
   Rule-oriented query helpers over Ast views.

   These are intentionally small wrappers around the shared Ast traversal
   traversal so individual rules can start from semantic-ish collections
   instead of unpacking `ctx.source_file` manually.
*)
val structure_items: Rule.context -> Syn.Ast.StructureItem.t list

val signature_items: Rule.context -> Syn.Ast.SignatureItem.t list

val expressions: Rule.context -> Syn.Ast.Expr.t list

val let_bindings: Rule.context -> Syn.Ast.LetBinding.t list

val type_declarations: Rule.context -> Syn.Ast.TypeDeclaration.t list
