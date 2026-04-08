open Std

type t
val empty: t

val of_type_decls:
  ?cached_by_id:(TypeConstructorId.t, FileSummary.type_decl) Collections.HashMap.t ->
  FileSummary.type_decl list ->
  t

val merge: t -> t -> t

val bind: t -> FileSummary.type_decl list -> t

val type_decls: t -> FileSummary.type_decl list

val by_id: t -> (TypeConstructorId.t, FileSummary.type_decl) Collections.HashMap.t

val lookup: t -> IdentPath.t -> FileSummary.type_decl option

val lookup_by_id: t -> TypeConstructorId.t -> FileSummary.type_decl option

val resolve_named_type_head: t -> IdentPath.t -> TypeRepr.named_type_head option

val canonicalize_type: t -> TypeRepr.t -> TypeRepr.t

val canonicalize_scheme: t -> TypeScheme.t -> TypeScheme.t

val canonicalize_inline_record_labels: t -> TypeDecl.label list -> TypeDecl.label list

val canonicalize_type_decl: t -> FileSummary.type_decl -> FileSummary.type_decl

val type_decls_for_include: t -> IdentPath.t -> FileSummary.type_decl list

val type_decls_for_module_alias:
  t -> alias_name:string -> module_path:IdentPath.t -> FileSummary.type_decl list
