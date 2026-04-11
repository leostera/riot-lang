open Std
open Model

type opened_module = {
  visible_path: SurfacePath.t;
  module_id: PackageEnv.ModuleId.t;
}

type resolved_module = {
  visible_path: SurfacePath.t;
  module_id: PackageEnv.ModuleId.t;
  suffix: SurfacePath.t;
}

type t

val empty: unit -> t

val create: package_env:PackageEnv.t -> scope_view:ScopeView.t -> t

val package_env: t -> PackageEnv.t

val scope_view: t -> ScopeView.t

val resolve_visible_module_prefix: t -> SurfacePath.t -> resolved_module option

val implicit_open_modules: t -> opened_module list

val visible_modules: t -> (SurfacePath.t * PackageEnv.ModuleId.t) list

val visible_type_decls: t -> FileSummary.type_decl list

val visible_type_decl_by_id: t -> TypeConstructorId.t -> FileSummary.type_decl option

val lookup_value: t -> EntityId.t -> TypeScheme.t option

val lookup_module_scope: t -> SurfacePath.t -> CompiledScope.t option

val lookup_type_decl: t -> SurfacePath.t -> FileSummary.type_decl option

val visible_type_decls_for_module:
  t -> visible_path:SurfacePath.t -> module_id:PackageEnv.ModuleId.t -> FileSummary.type_decl list
