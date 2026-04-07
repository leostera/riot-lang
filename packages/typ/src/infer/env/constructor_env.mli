open Std
open Model

type entry
type t
val empty: t

val of_type_decls: FileSummary.type_decl list -> t

val bind: t -> t -> t

val entries: t -> entry list

val lookup_all: t -> string -> entry list

val name: entry -> string

val constructor_id: entry -> ConstructorId.t

val owner_path: entry -> IdentPath.t

val owner_type_constructor_id: entry -> TypeConstructorId.t

val scheme: entry -> TypeScheme.t
