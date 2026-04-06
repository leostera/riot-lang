open Std
open Analysis
open Model

type t = Binding.t list

type scope_entries = (IdentPath.t * t) list

type scope_opens = (IdentPath.t * IdentPath.t list) list

val of_entries: provenance:Binding.provenance -> TypConfig.env -> t

val singleton: name:string -> scheme:TypeScheme.t -> provenance:Binding.provenance -> t

val unique: t -> t

val render: t -> Check_result.env

val visible_entries: t -> t

val lookup: t -> IdentPath.t -> Binding.t option

val lookup_all: t -> IdentPath.t -> Binding.t list

val names: t -> string list

val introduced_names: t -> t -> string list

val bind: t -> t -> t

val with_local_open: t -> IdentPath.t -> t

val entries_for_include: t -> IdentPath.t -> t

val export_names_for_module_alias: t -> alias_name:string -> module_path:IdentPath.t -> string list

val entries_for_module_alias: t -> alias_name:string -> module_path:IdentPath.t -> t

val export: TypConfig.t -> t -> t

val export_with_forced_names: State.t -> t -> t

val introduced_entries: t -> t -> t

val qualify_entries: IdentPath.t -> t -> t

val update_scope_entries: scope_entries -> IdentPath.t -> t -> scope_entries

val update_scope_opens: scope_opens -> IdentPath.t -> IdentPath.t -> scope_opens

val for_item_scope: t -> scope_entries -> scope_opens -> IdentPath.t -> t
