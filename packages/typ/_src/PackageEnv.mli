open Std
open Model

module ModuleId : sig
  type t =
    | Loaded of LocalModules.RequiredName.t
    | Local of LocalModules.InternalName.t
end

type t

val empty: unit -> t

val of_loaded_modules: LoadedModules.t -> t

val add_loaded: t -> required_name:LocalModules.RequiredName.t -> ModuleTypings.t -> unit

val add_local: t -> internal_name:LocalModules.InternalName.t -> ModuleTypings.t -> unit

val find_artifact: t -> ModuleId.t -> ModuleTypings.t option

val find_loaded: t -> required_name:LocalModules.RequiredName.t -> ModuleTypings.t option

val find_local: t -> internal_name:LocalModules.InternalName.t -> ModuleTypings.t option

val find_compiled_scope: t -> ModuleId.t -> CompiledScope.t option

val lookup_value: t -> ModuleId.t -> SurfacePath.t -> TypeScheme.t option

val lookup_module_scope: t -> ModuleId.t -> SurfacePath.t -> CompiledScope.t option

val lookup_type_decl: t -> ModuleId.t -> SurfacePath.t -> FileSummary.type_decl option

val lookup_type_decl_by_id: t -> ModuleId.t -> TypeConstructorId.t -> FileSummary.type_decl option

val lookup_constructors: t -> ModuleId.t -> SurfacePath.t -> CompiledScope.constructor_entry list

val lookup_owned_constructor: t -> ModuleId.t -> SurfacePath.t -> TypeConstructorId.t -> CompiledScope.constructor_entry option

val lookup_record_decls: t -> ModuleId.t -> string -> CompiledScope.record_decl list

val lookup_record_decl_by_owner: t -> ModuleId.t -> TypeConstructorId.t -> CompiledScope.record_decl option

val visible_type_decls: t -> (SurfacePath.t * ModuleId.t) list -> FileSummary.type_decl list

val visible_type_decl_by_id: t -> ModuleId.t list -> TypeConstructorId.t -> FileSummary.type_decl option
