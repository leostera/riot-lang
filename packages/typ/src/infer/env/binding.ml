open Std
open Model

type ident = {
  local_id: int;
  name: string;
}

let make_ident = fun ~local_id ~name -> { local_id; name }

let ident_name = fun ident -> ident.name

let ident_local_id = fun ident -> ident.local_id

let same_ident = fun left right ->
  Int.equal left.local_id right.local_id

let compare_ident = fun left right ->
  Int.compare left.local_id right.local_id

type provenance =
  | LoweredPattern of PatId.t
  | Prelude
  | Ambient
  | TypeConstructor of { type_name: string; scope_path: IdentPath.t }
  | Exception of { name: string; scope_path: IdentPath.t }
  | DeclaredValue of { name: string; scope_path: IdentPath.t }
  | Included of { module_path: IdentPath.t }
  | ModuleAlias of { alias_name: string; module_path: IdentPath.t }

type t = {
  ident: ident;
  name: string;
  path: IdentPath.t;
  scheme: TypeScheme.t;
  provenance: provenance;
}

let path_name = fun path ->
  match IdentPath.last_name path with
  | Some name -> name
  | None -> ""

let make = fun ~ident ~path ~scheme ~provenance ->
  {
    ident;
    name = path_name path;
    path;
    scheme;
    provenance;
  }

let ident = fun binding -> binding.ident

let same = fun left right -> same_ident left.ident right.ident

let compare = fun left right -> compare_ident left.ident right.ident

let name = fun binding -> binding.name

let path = fun binding -> binding.path

let scheme = fun binding -> binding.scheme

let provenance = fun binding -> binding.provenance

let with_path = fun path binding -> { binding with name = path_name path; path }

let with_scheme = fun scheme binding -> { binding with scheme }

let with_provenance = fun provenance binding -> { binding with provenance }

let render = fun binding -> (IdentPath.to_string binding.path, binding.scheme)
