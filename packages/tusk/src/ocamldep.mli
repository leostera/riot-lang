(** OCamldep wrapper - handles dependency analysis *)

(** Sort ML/MLI files in dependency order *)
val sort : toolchain:Toolchains.t -> cwd:string -> files:string list -> string list

(** Get dependencies for a single file *)
val deps : toolchain:Toolchains.t -> cwd:string -> file:string -> string list

(** Get all module dependencies (for building .merlin files) *)
val all_deps : toolchain:Toolchains.t -> cwd:string -> files:string list -> (string * string list) list