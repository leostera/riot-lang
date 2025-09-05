(** OCamldep wrapper - handles dependency analysis *)

val sort :
  toolchain:Toolchains.toolchain ->
  cwd:string ->
  files:string list ->
  string list
(** Sort ML/MLI files in dependency order *)

val deps :
  toolchain:Toolchains.toolchain ->
  cwd:string ->
  file:string ->
  package_namespace:Mod_name.namespace ->
  Mod_name.t list
(** Get dependencies for a single file - returns Mod_name.t list *)

val deps_with_flags :
  toolchain:Toolchains.toolchain ->
  cwd:string ->
  file:string ->
  flags:string ->
  package_namespace:Mod_name.namespace ->
  Mod_name.t list
(** Get dependencies for a single file with additional ocamldep flags - returns
    Mod_name.t list *)

val all_deps :
  toolchain:Toolchains.toolchain ->
  cwd:string ->
  files:string list ->
  package_namespace:Mod_name.namespace ->
  (string * Mod_name.t list) list
(** Get all module dependencies (for building .merlin files) *)
