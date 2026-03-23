open Std

(** Defaultable visitor-style traversal over the typed CST.

    This sits one layer above {!Traversal}. Rule authors can provide only the
    callbacks they care about and rely on the shared walker for recursion.
*)

type 'ctx control =
  | Continue of 'ctx
  | Skip_children of 'ctx
  | Stop of 'ctx

type 'ctx visitor = {
  enter_structure_item :
    'ctx -> Cst.StructureItem.t -> 'ctx control;
  enter_signature_item :
    'ctx -> Cst.SignatureItem.t -> 'ctx control;
  enter_let_binding :
    'ctx -> Cst.LetBinding.t -> 'ctx control;
  enter_type_declaration :
    'ctx -> Cst.TypeDeclaration.t -> 'ctx control;
  enter_expression :
    'ctx -> Cst.Expression.t -> 'ctx control;
  enter_core_type :
    'ctx -> Cst.CoreType.t -> 'ctx control;
}

val default : 'ctx visitor

val source_file : 'ctx visitor -> 'ctx -> Cst.source_file -> 'ctx
val structure_item : 'ctx visitor -> 'ctx -> Cst.StructureItem.t -> 'ctx
val signature_item : 'ctx visitor -> 'ctx -> Cst.SignatureItem.t -> 'ctx
val let_binding : 'ctx visitor -> 'ctx -> Cst.LetBinding.t -> 'ctx
val type_declaration : 'ctx visitor -> 'ctx -> Cst.TypeDeclaration.t -> 'ctx
val expression : 'ctx visitor -> 'ctx -> Cst.Expression.t -> 'ctx
val core_type : 'ctx visitor -> 'ctx -> Cst.CoreType.t -> 'ctx
