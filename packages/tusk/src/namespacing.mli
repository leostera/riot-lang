(** Module namespacing utilities for tusk build system *)

val snake_to_camel : string -> string
(** Convert snake_case to CamelCase Example: "hello_world" -> "HelloWorld" *)

val path_to_module_name : package_name:string -> string -> string
(** Convert a file path to a namespaced module name Examples:
    - "hello_world.ml" -> "HelloWorld"
    - "a/b/c.ml" -> "A__B__C"
    - "a/hello_world.ml" -> "A__HelloWorld" *)

val get_module_name : package_name:string -> string -> string
(** Get the transformed module name for a source file *)

val get_module_name_with_folders : package_name:string -> namespace:string list -> string -> string
(** Get the transformed module name with folder-based namespacing support *)

val generate_package_module :
  package_name:string -> modules:(string * string) list -> string
(** Generate a module alias file that re-exports all modules in a package *)

val get_flat_filename : package_name:string -> string -> string
(** Get the flattened filename for compilation Example: "a/b/hello_world.ml" ->
    "pkg__a__b__hello_world.ml" *)
