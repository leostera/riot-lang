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

type t = {
  path: IdentPath.t;
  scheme: TypeScheme.t;
  provenance: provenance;
}

let make = fun ~path ~scheme ~provenance -> { path; scheme; provenance }

let path = fun binding -> binding.path

let scheme = fun binding -> binding.scheme

let provenance = fun binding -> binding.provenance

let with_path = fun path binding -> { binding with path }

let with_scheme = fun scheme binding -> { binding with scheme }

let render = fun binding -> (IdentPath.to_string binding.path, binding.scheme)
