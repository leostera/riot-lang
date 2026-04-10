open Std
open Model

type ident = {
  local_id: int;
  name: string;
}
type provenance =
  | LoweredPattern of PatId.t
  | Prelude
  | Ambient
  | TypeConstructor of { type_name: string; scope_path: IdentPath.t }
  | Exception of { name: string; scope_path: IdentPath.t }
  | DeclaredValue of { name: string; scope_path: IdentPath.t }
  | Included of { module_path: IdentPath.t }
  | ModuleAlias of { alias_name: string; module_path: IdentPath.t }
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
  | BindInScope of t * IdentPath.t * t
  | Open of t * IdentPath.t
  | Qualify of t * IdentPath.t
val empty: t

val snapshot: bindings:bindings -> type_decls:FileSummary.type_decl list -> t

val bind: t -> t -> t

val bind_in_scope: t -> scope_path:IdentPath.t -> t -> t

val open_: t -> IdentPath.t -> t

val qualify: t -> scope_path:IdentPath.t -> t
