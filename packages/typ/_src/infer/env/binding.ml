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

type t = {
  id: BindingId.t;
  name: string;
  path: EntityId.t;
  scheme: TypeScheme.t;
  provenance: provenance;
}

let path_name = fun surface_path ->
  match SurfacePath.last_name surface_path with
  | Some name -> name
  | None -> ""

let make = fun ~id ~surface_path ~scheme ~provenance ->
  {
    id;
    name = path_name surface_path;
    path = EntityId.resolved ~binding_id:id ~surface_path;
    scheme;
    provenance;
  }

let with_path = fun path binding -> {
  binding with
  name = path_name (EntityId.surface_path path);
  path;
}

let id = fun binding -> binding.id

let same = fun left right -> BindingId.equal left.id right.id

let compare = fun left right -> BindingId.compare left.id right.id

let name = fun binding -> binding.name

let path = fun binding -> binding.path

let surface_path = fun binding -> EntityId.surface_path binding.path

let scheme = fun binding -> binding.scheme

let provenance = fun binding -> binding.provenance

let with_surface_path = fun surface_path binding ->
  with_path
    (EntityId.resolved ~binding_id:binding.id ~surface_path)
    binding

let with_scheme = fun scheme binding -> { binding with scheme }

let with_provenance = fun provenance binding -> { binding with provenance }

let render = fun binding -> (SurfacePath.to_string (surface_path binding), binding.scheme)
