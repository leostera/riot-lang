module Legacy_env = Env

open Std
open Analysis
open Model

type t
type scope

val empty: t

val empty_scope: scope

val summary_snapshot: t -> Summary2.t

val env_of_summary: Summary2.t -> t

val env_of_legacy_summary: Legacy_env.summary -> t

val of_legacy_env: Legacy_env.t -> t

val to_legacy_env: t -> Legacy_env.t

val of_bindings: Legacy_env.Binding.t list -> t

val of_type_decls: FileSummary.type_decl list -> t

val bind: t -> t -> t

val bind_in_scope: t -> scope_path:IdentPath.t -> t -> t

val with_local_open: t -> IdentPath.t -> t

val qualify: scope_path:IdentPath.t -> t -> t

val lookup_module_scope: t -> IdentPath.t -> scope option

val lookup: t -> IdentPath.t -> Legacy_env.Binding.t option

val lookup_all: t -> IdentPath.t -> Legacy_env.Binding.t list

val lookup_type: t -> IdentPath.t -> FileSummary.type_decl option

val lookup_constructors: t -> IdentPath.t -> Legacy_env.Constructor_env.entry list

val lookup_owned_constructor: t -> IdentPath.t -> TypeConstructorId.t -> Legacy_env.Constructor_env.entry option

val lookup_record_decls: t -> string -> Legacy_env.Label_env.record_decl list

val lookup_record_decl_by_owner: t -> TypeConstructorId.t -> Legacy_env.Label_env.record_decl option

val bindings: t -> Legacy_env.Binding.t list

val type_decls: t -> FileSummary.type_decl list

val record_decls: t -> Legacy_env.Label_env.record_decl list

val scope_values: scope -> Legacy_env.Value_env.t

val scope_types: scope -> Legacy_env.Type_env.t

val scope_constructors: scope -> Legacy_env.Constructor_env.t

val scope_labels: scope -> Legacy_env.Label_env.t
