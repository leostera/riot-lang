type toolchain

val default_ocaml_version : string
val default_toolchain : toolchain
val get_toolchain_path : toolchain -> string
val get_version : toolchain -> string
val is_toolchain_installed : toolchain -> bool
val list_installed_toolchains : unit -> string list
val ocamlc_path : toolchain -> string
val ocamldep_path : toolchain -> string
val ocamlopt_path : toolchain -> string
val ready_toolchains : Workspace.workspace -> toolchain
val toolchain_base_dir : string
val validate_toolchain : toolchain -> bool
