module Legacy_env = Env

open Std
open Model

type bindings = Legacy_env.Binding.t list

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

val empty: t

val snapshot: bindings:bindings -> type_decls:FileSummary.type_decl list -> t

val bind: t -> t -> t

val bind_in_scope: t -> scope_path:IdentPath.t -> t -> t

val open_: t -> IdentPath.t -> t

val qualify: t -> scope_path:IdentPath.t -> t

val of_legacy_summary: Legacy_env.summary -> t

val to_legacy_summary: t -> Legacy_env.summary
