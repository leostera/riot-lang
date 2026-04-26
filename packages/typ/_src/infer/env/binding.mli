open Std
open Model

type provenance =
  | LoweredPattern of PatternArenaId.t
  | Prelude
  | Ambient
  | TypeConstructor of {
      type_name: string;
      scope_path: SurfacePath.t;
    }
  | Exception of {
      name: string;
      scope_path: SurfacePath.t;
    }
  | DeclaredValue of {
      name: string;
      scope_path: SurfacePath.t;
    }
  | Included of {
      module_path: SurfacePath.t;
    }
  | ModuleAlias of {
      alias_name: string;
      module_path: SurfacePath.t;
    }
type t
val make:
  id:BindingId.t ->
  surface_path:SurfacePath.t ->
  scheme:TypeScheme.t ->
  provenance:provenance ->
  t

val with_path: EntityId.t -> t -> t

val id: t -> BindingId.t

val same: t -> t -> bool

val compare: t -> t -> int

val name: t -> string

val path: t -> EntityId.t

val surface_path: t -> SurfacePath.t

val scheme: t -> TypeScheme.t

val provenance: t -> provenance

val with_surface_path: SurfacePath.t -> t -> t

val with_scheme: TypeScheme.t -> t -> t

val with_provenance: provenance -> t -> t

val render: t -> string * TypeScheme.t
