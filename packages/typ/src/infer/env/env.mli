open Std
open Analysis
open Model

module Binding: module type of Binding

module Module_env: module type of Module_env

module Type_env: module type of Type_env

module Constructor_env: module type of Constructor_env

module Label_env: module type of Label_env

module Value_env: module type of Value_env

type bindings = Binding.t list
type t
type scope
type summary_delta = {
  bindings: bindings;
  type_decls: FileSummary.type_decl list;
}
type summary =
  | Summary_empty
  | Summary_snapshot of summary_delta
  | Summary_bind of summary * summary
  | Summary_bind_in_scope of summary * IdentPath.t * summary
  | Summary_open of summary * IdentPath.t
  | Summary_qualify of summary * IdentPath.t

val empty_summary: summary
val summary_snapshot: t -> summary
val summary_bind: summary -> t -> summary
val summary_bind_in_scope: summary -> scope_path:IdentPath.t -> t -> summary
val summary_open: summary -> IdentPath.t -> summary
val summary_qualify: summary -> scope_path:IdentPath.t -> summary
val env_of_summary: summary -> t
val empty: t

val empty_scope: scope

val of_type_decls: FileSummary.type_decl list -> t

val of_entries:
  make_ident:(string -> Binding.ident) -> provenance:Binding.provenance -> TypConfig.env -> t

val of_bindings: bindings -> t

val singleton:
  make_ident:(string -> Binding.ident) ->
  name:string ->
  scheme:TypeScheme.t ->
  provenance:Binding.provenance ->
  t

val bindings: t -> bindings

val type_decls: t -> FileSummary.type_decl list

val types: t -> Type_env.t

val unique: t -> t

val render: t -> Check_result.env

val lookup: t -> IdentPath.t -> Binding.t option

val lookup_all: t -> IdentPath.t -> bindings

val lookup_constructors: t -> IdentPath.t -> Constructor_env.entry list

val lookup_record_decls: t -> string -> Label_env.record_decl list

val record_decls: t -> Label_env.record_decl list

val names: t -> string list

val introduced_names: t -> t -> string list

val bind: t -> t -> t

val bind_in_scope: t -> scope_path:IdentPath.t -> t -> t

val extend: t -> bindings -> t

val with_local_open: t -> IdentPath.t -> t

val entries_for_include: t -> IdentPath.t -> t

val export_names_for_module_alias: t -> alias_name:string -> module_path:IdentPath.t -> string list

val entries_for_module_alias: t -> alias_name:string -> module_path:IdentPath.t -> t

val export: TypConfig.t -> t -> t

val export_with_forced_names: config:TypConfig.t -> forced_export_names:string list -> t -> t

val introduced_entries: t -> t -> t

val qualify: scope_path:IdentPath.t -> t -> t

val register_entries: scope -> scope_path:IdentPath.t -> t -> scope

val register_open: scope -> scope_path:IdentPath.t -> module_path:IdentPath.t -> scope

val for_item_scope: t -> scope -> scope_path:IdentPath.t -> t
