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
type binding = {
  ident: BindingId.t;
  path: EntityId.t;
  scheme: TypeScheme.t;
  provenance: provenance;
}
type bindings = binding list
type delta = {
  bindings: bindings;
  type_decls: FileSummary.type_decl list;
}
type t =
  | Empty
  | Snapshot of delta
  | Bind of t * t
  | BindInScope of t * SurfacePath.t * t
  | Open of t * SurfacePath.t
  | Qualify of t * SurfacePath.t
val empty: t

val snapshot: bindings:bindings -> type_decls:FileSummary.type_decl list -> t

val bind: t -> t -> t

val bind_in_scope: t -> scope_path:SurfacePath.t -> t -> t

val open_: t -> SurfacePath.t -> t

val qualify: t -> scope_path:SurfacePath.t -> t
