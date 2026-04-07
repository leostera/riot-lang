open Std
open Model

type ident = {
  local_id: int;
  name: string;
}

type provenance =
  | Lowered_pattern of PatId.t
  | Prelude
  | Ambient
  | Type_constructor of { type_name: string; scope_path: IdentPath.t }
  | Exception of { name: string; scope_path: IdentPath.t }
  | Declared_value of { name: string; scope_path: IdentPath.t }
  | Included of { module_path: IdentPath.t }
  | Module_alias of { alias_name: string; module_path: IdentPath.t }

type binding = {
  ident: ident;
  path: IdentPath.t;
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
  | Bind_in_scope of t * IdentPath.t * t
  | Open of t * IdentPath.t
  | Qualify of t * IdentPath.t

let empty = Empty

let snapshot = fun ~bindings ~type_decls -> Snapshot { bindings; type_decls }

let bind = fun summary introduced -> Bind (summary, introduced)

let bind_in_scope = fun summary ~scope_path introduced ->
  Bind_in_scope (summary, scope_path, introduced)

let open_ = fun summary module_path -> Open (summary, module_path)

let qualify = fun summary ~scope_path -> Qualify (summary, scope_path)
