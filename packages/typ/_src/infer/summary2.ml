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

let empty = Empty

let snapshot = fun ~bindings ~type_decls -> Snapshot { bindings; type_decls }

let bind = fun summary introduced -> Bind (summary, introduced)

let bind_in_scope = fun summary ~scope_path introduced ->
  BindInScope (summary, scope_path, introduced)

let open_ = fun summary module_path -> Open (summary, module_path)

let qualify = fun summary ~scope_path -> Qualify (summary, scope_path)
