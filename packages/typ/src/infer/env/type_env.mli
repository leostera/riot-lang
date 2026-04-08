open Std
open Model

type t
val empty: t

val of_type_decls: FileSummary.type_decl list -> t

val local_only: t -> t

val type_decls: t -> FileSummary.type_decl list

val visible_type_decls: t -> FileSummary.type_decl list

val bind: t -> t -> t

val add_open: root:IdentPath.t -> t -> t -> t

val lookup: t -> IdentPath.t -> FileSummary.type_decl option

val lookup_by_id: t -> TypeConstructorId.t -> FileSummary.type_decl option

val qualify_entries: IdentPath.t -> t -> t

val entries_for_include: t -> IdentPath.t -> t

val entries_for_module_alias: t -> alias_name:string -> module_path:IdentPath.t -> t
