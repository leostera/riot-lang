open Std

type module_binding = {
  name: string;
  scope: t;
}

and t = {
  values: (string * TypeScheme.t) list;
  type_decls: FileSummary.type_decl list;
  modules: module_binding list;
}

val empty: t

val of_module_surface: exports:FileSummary.exports -> type_decls:FileSummary.type_decl list -> t

val exports: t -> FileSummary.exports

val type_decls: t -> FileSummary.type_decl list
