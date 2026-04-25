open Std

type constructor_entry = {
  owner_path: SurfacePath.t;
  owner_type_constructor_id: TypeConstructorId.t;
  constructor: TypeDecl.constructor;
}

type record_decl = {
  owner_path: SurfacePath.t;
  owner_type_constructor_id: TypeConstructorId.t;
  param_ids: int list;
  labels: TypeDecl.label list;
}

type t

val empty: t

val of_module_surface: exports:FileSummary.exports -> type_decls:FileSummary.type_decl list -> t

val exports: t -> FileSummary.exports

val type_decls: t -> FileSummary.type_decl list

val lookup_value: t -> SurfacePath.t -> TypeScheme.t option

val lookup_module: t -> SurfacePath.t -> t option

val lookup_type_decl: t -> SurfacePath.t -> FileSummary.type_decl option

val lookup_constructors: t -> SurfacePath.t -> constructor_entry list

val lookup_owned_constructor: t -> SurfacePath.t -> TypeConstructorId.t -> constructor_entry option

val lookup_record_decls: t -> string -> record_decl list

val lookup_record_decl_by_owner: t -> TypeConstructorId.t -> record_decl option
