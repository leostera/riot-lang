open Std
open Model

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

val make: path:IdentPath.t -> scheme:TypeScheme.t -> provenance:provenance -> t

val path: t -> IdentPath.t

val scheme: t -> TypeScheme.t

val provenance: t -> provenance

val with_path: IdentPath.t -> t -> t

val with_scheme: TypeScheme.t -> t -> t

val render: t -> string * TypeScheme.t
