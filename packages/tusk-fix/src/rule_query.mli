open Std

(** Rule-oriented query helpers over the typed CST.

    These are intentionally small wrappers around the shared `Syn.Visit`
    traversal so individual rules can start from semantic-ish collections
    instead of unpacking `ctx.cst` manually.
*)

val structure_items : Rule.context -> Syn.Cst.StructureItem.t list
val signature_items : Rule.context -> Syn.Cst.SignatureItem.t list
val expressions : Rule.context -> Syn.Cst.Expression.t list
val let_bindings : Rule.context -> Syn.Cst.LetBinding.t list
val type_declarations : Rule.context -> Syn.Cst.TypeDeclaration.t list
