(** OCamldep wrapper - handles dependency analysis *)

val sort :
  toolchain:Toolchains.toolchain ->
  cwd:string ->
  files:string list ->
  string list
(** Sort ML/MLI files in dependency order *)

val deps :
  toolchain:Toolchains.toolchain -> cwd:string -> file:string -> string list
(** Get dependencies for a single file *)

val all_deps :
  toolchain:Toolchains.toolchain ->
  cwd:string ->
  files:string list ->
  (string * string list) list
(** Get all module dependencies (for building .merlin files) *)
