open Std
open Model

type record_decl
type t
val empty: t

val of_type_decls: FileSummary.type_decl list -> t

val local_only: t -> t

val bind: t -> t -> t

val add_open: root:IdentPath.t -> t -> t -> t

val record_decls: t -> record_decl list

val visible_record_decls: t -> record_decl list

val lookup_all: t -> string -> record_decl list

val lookup_name: string -> string

val owner_path: record_decl -> IdentPath.t

val owner_type_constructor_id: record_decl -> TypeConstructorId.t

val param_ids: record_decl -> int list

val labels: record_decl -> TypeDecl.label list

val field: record_decl -> string -> TypeDecl.label option

val field_names: record_decl -> string list

val matches_fields: record_decl -> string list -> bool
