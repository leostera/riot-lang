open Std
open Model

val type_decl_key: FileSummary.type_decl -> IdentPath.t

val qualify_scheme:
  type_decls:FileSummary.type_decl list -> module_name:string -> TypeScheme.t -> TypeScheme.t

val qualify_type_decls: module_name:string -> FileSummary.type_decl list -> FileSummary.type_decl list

val qualify_exports:
  module_name:string ->
  type_decls:FileSummary.type_decl list ->
  FileSummary.exports ->
  (IdentPath.t * TypeScheme.t) list

val qualify_signature_exports:
  module_name:string -> type_decls:FileSummary.type_decl list -> FileSummary.exports -> FileSummary.exports
