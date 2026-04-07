open Std
open Model

type record_decl = {
  owner_path: IdentPath.t;
  owner_type_constructor_id: TypeConstructorId.t;
  param_ids: int list;
  labels: TypeDecl.label list;
}
type t
val empty: t

val of_type_decls: FileSummary.type_decl list -> t

val bind: t -> t -> t

val record_decls: t -> record_decl list

val lookup_all: t -> string -> record_decl list
