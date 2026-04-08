open Std
open Analysis
open Model

module Binding: module type of Binding

module Constructor_env: module type of Constructor_env

module Label_env: module type of Label_env

module Type_env: module type of Type_env

module Value_env: module type of Value_env

type bindings = Binding.t list
type summary = Summary2.t
type t
type module_scope
type item_scope
val empty: t

val empty_summary: summary

val empty_item_scope: item_scope

val summary_snapshot: t -> summary

val summary_bind: summary -> t -> summary

val summary_bind_in_scope: summary -> scope_path:IdentPath.t -> t -> summary

val summary_open: summary -> IdentPath.t -> summary

val summary_qualify: summary -> scope_path:IdentPath.t -> summary

val env_of_summary: summary -> t

val of_entries:
  make_ident:(string -> Binding.ident) -> provenance:Binding.provenance -> TypConfig.env -> t

val of_bindings: bindings -> t

val of_type_decls: FileSummary.type_decl list -> t

val singleton:
  make_ident:(string -> Binding.ident) ->
  name:string ->
  scheme:TypeScheme.t ->
  provenance:Binding.provenance ->
  t

val singleton_constructor:
  make_ident:(string -> Binding.ident) ->
  name:string ->
  scheme:TypeScheme.t ->
  provenance:Binding.provenance ->
  owner_path:IdentPath.t ->
  owner_type_constructor_id:TypeConstructorId.t ->
  constructor_id:ConstructorId.t ->
  inline_record_labels:TypeDecl.label list option ->
  t

val bindings: t -> bindings

val type_decls: t -> FileSummary.type_decl list

val visible_type_decls: t -> FileSummary.type_decl list

val types: t -> Type_env.t

val bind: t -> t -> t

val extend: t -> bindings -> t

val bind_in_scope: t -> scope_path:IdentPath.t -> t -> t

val with_local_open: t -> IdentPath.t -> t

val qualify: scope_path:IdentPath.t -> t -> t

val lookup_module_scope: t -> IdentPath.t -> module_scope option

val lookup: t -> IdentPath.t -> Binding.t option

val lookup_all: t -> IdentPath.t -> bindings

val lookup_type: t -> IdentPath.t -> FileSummary.type_decl option

val lookup_constructors: t -> IdentPath.t -> Constructor_env.entry list

val lookup_owned_constructor: t -> IdentPath.t -> TypeConstructorId.t -> Constructor_env.entry option

val lookup_record_decls: t -> string -> Label_env.record_decl list

val lookup_record_decl_by_owner: t -> TypeConstructorId.t -> Label_env.record_decl option

val record_decls: t -> Label_env.record_decl list

val unique: t -> t

val render: t -> Check_result.env

val names: t -> string list

val introduced_names: t -> t -> string list

val export: TypConfig.t -> t -> t

val export_with_forced_names: config:TypConfig.t -> forced_export_names:string list -> t -> t

val introduced_entries: t -> t -> t

val entries_for_include: t -> IdentPath.t -> t

val export_names_for_module_alias: t -> alias_name:string -> module_path:IdentPath.t -> string list

val entries_for_module_alias: t -> alias_name:string -> module_path:IdentPath.t -> t

val register_entries: item_scope -> scope_path:IdentPath.t -> t -> item_scope

val register_open: item_scope -> scope_path:IdentPath.t -> module_path:IdentPath.t -> item_scope

val for_item_scope: t -> item_scope -> scope_path:IdentPath.t -> t

val scope_values: module_scope -> Value_env.t

val scope_types: module_scope -> Type_env.t

val scope_constructors: module_scope -> Constructor_env.t

val scope_labels: module_scope -> Label_env.t
