open Std
open Model

type entry
type t
val empty: t

val of_type_decls: FileSummary.type_decl list -> t

val singleton:
  owner_path:IdentPath.t ->
  owner_type_constructor_id:TypeConstructorId.t ->
  constructor:TypeDecl.constructor ->
  t

val local_only: t -> t

val bind: t -> t -> t

val add_open: root:IdentPath.t -> type_decls:FileSummary.type_decl list -> t -> t -> t

val entries: t -> entry list

val lookup_all: t -> string -> entry list

val lookup_owned: t -> string -> TypeConstructorId.t -> entry option

val name: entry -> string

val constructor_id: entry -> ConstructorId.t

val owner_path: entry -> IdentPath.t

val owner_type_constructor_id: entry -> TypeConstructorId.t

val scheme: entry -> TypeScheme.t

val inline_record_labels: entry -> TypeDecl.label list option

val qualify_entry: root:IdentPath.t -> type_decls:FileSummary.type_decl list -> entry -> entry
