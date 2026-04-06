open Std
open Analysis
open Model

module Binding: module type of Binding
module Value_env: module type of Value_env

type bindings = Binding.t list

type t

type scope

val empty: t

val empty_scope: scope

val of_entries: provenance:Binding.provenance -> TypConfig.env -> t

val of_bindings: bindings -> t

val singleton: name:string -> scheme:TypeScheme.t -> provenance:Binding.provenance -> t

val bindings: t -> bindings

val unique: t -> t

val render: t -> Check_result.env

val lookup: t -> IdentPath.t -> Binding.t option

val lookup_all: t -> IdentPath.t -> bindings

val names: t -> string list

val introduced_names: t -> t -> string list

val bind: t -> t -> t

val extend: t -> bindings -> t

val with_local_open: t -> IdentPath.t -> t

val entries_for_include: t -> IdentPath.t -> t

val export_names_for_module_alias: t -> alias_name:string -> module_path:IdentPath.t -> string list

val entries_for_module_alias: t -> alias_name:string -> module_path:IdentPath.t -> t

val export: TypConfig.t -> t -> t

val export_with_forced_names: State.t -> t -> t

val introduced_entries: t -> t -> t

val qualify: scope_path:IdentPath.t -> t -> t

val register_entries: scope -> scope_path:IdentPath.t -> t -> scope

val register_open: scope -> scope_path:IdentPath.t -> module_path:IdentPath.t -> scope

val for_item_scope: t -> scope -> scope_path:IdentPath.t -> t
