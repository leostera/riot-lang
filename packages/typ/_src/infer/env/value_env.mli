open Std
open Analysis
open Model

type t

val empty: t

val of_bindings: Binding.t list -> t

val find_same: t -> BindingId.t -> Binding.t option

val local_only: t -> t

val of_entries: make_id:(SurfacePath.t -> BindingId.t) -> provenance:Binding.provenance -> TypConfig.env -> t

val singleton: make_id:(SurfacePath.t -> BindingId.t) -> name:string -> scheme:TypeScheme.t -> provenance:Binding.provenance -> t

val bindings: t -> Binding.t list

val canonicalize: t -> t

val unique: t -> t

val render: t -> Check_result.env

val visible_entries: t -> t

val lookup: t -> EntityId.t -> Binding.t option

val lookup_all: t -> EntityId.t -> Binding.t list

val names: t -> string list

val introduced_names: t -> t -> string list

val bind: t -> t -> t

val add_open: root:SurfacePath.t -> t -> t -> t

val with_local_open: t -> SurfacePath.t -> t

val entries_for_include: t -> SurfacePath.t -> t

val export_names_for_module_alias: t -> alias_name:string -> module_path:SurfacePath.t -> string list

val entries_for_module_alias: t -> alias_name:string -> module_path:SurfacePath.t -> t

val export: TypConfig.t -> t -> t

val export_with_forced_names: config:TypConfig.t -> forced_export_names:string list -> t -> t

val introduced_entries: t -> t -> t

val qualify_entries: SurfacePath.t -> t -> t
