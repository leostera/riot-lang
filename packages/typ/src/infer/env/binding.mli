open Std
open Model

type ident
val make_ident: local_id:int -> name:string -> ident

val ident_name: ident -> string

val ident_local_id: ident -> int

val same_ident: ident -> ident -> bool

val compare_ident: ident -> ident -> int

type provenance =
  | Lowered_pattern of PatId.t
  | Prelude
  | Ambient
  | Type_constructor of { type_name: string; scope_path: IdentPath.t }
  | Exception of { name: string; scope_path: IdentPath.t }
  | Declared_value of { name: string; scope_path: IdentPath.t }
  | Included of { module_path: IdentPath.t }
  | Module_alias of { alias_name: string; module_path: IdentPath.t }
type t
val make: ident:ident -> path:IdentPath.t -> scheme:TypeScheme.t -> provenance:provenance -> t

val ident: t -> ident

val same: t -> t -> bool

val compare: t -> t -> int

val name: t -> string

val path: t -> IdentPath.t

val scheme: t -> TypeScheme.t

val provenance: t -> provenance

val with_path: IdentPath.t -> t -> t

val with_scheme: TypeScheme.t -> t -> t

val with_provenance: provenance -> t -> t

val render: t -> string * TypeScheme.t
