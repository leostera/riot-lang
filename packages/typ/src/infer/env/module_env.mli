open Std
open Model

type scope
type t
val empty: t

val empty_scope: scope

val make_scope: values:Value_env.t -> modules:t -> scope

val scope_values: scope -> Value_env.t

val scope_modules: scope -> t

val scope_bindings: scope -> Binding.t list

val bindings: t -> Binding.t list

val of_bindings: Binding.t list -> t

val bind: t -> t -> t

val lookup: t -> IdentPath.t -> scope option

val merge_scope: t -> module_path:IdentPath.t -> scope -> t

val bind_alias: t -> alias_name:string -> scope -> t
