open Std
open Model

type qualified_surface = {
  exports: FileSummary.exports;
  type_decls: FileSummary.type_decl list;
}
val type_decl_key: FileSummary.type_decl -> SurfacePath.t

val qualify_surface:
  module_name:string -> type_decls:FileSummary.type_decl list -> FileSummary.exports -> qualified_surface

val qualify_scheme:
  type_decls:FileSummary.type_decl list -> module_name:string -> TypeScheme.t -> TypeScheme.t

val qualify_type_decls: module_name:string -> FileSummary.type_decl list -> FileSummary.type_decl list

val qualify_exports:
  module_name:string ->
  type_decls:FileSummary.type_decl list ->
  FileSummary.exports ->
  (SurfacePath.t * TypeScheme.t) list

val qualify_signature_exports:
  module_name:string -> type_decls:FileSummary.type_decl list -> FileSummary.exports -> FileSummary.exports

val qualify_signature_type_decls:
  module_name:string -> FileSummary.type_decl list -> FileSummary.type_decl list

val rebind_module_typings: module_name:string -> ModuleTypings.t -> ModuleTypings.t
