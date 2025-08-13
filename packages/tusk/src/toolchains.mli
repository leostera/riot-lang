type toolchain
val default_ocaml_version : string
val default_toolchain : toolchain
val toolchain_base_dir : string
val get_toolchain_path : toolchain -> string
val ocamlc_path : toolchain -> string
val ocamlopt_path : toolchain -> string
val ocamldep_path : toolchain -> string
val is_toolchain_installed : toolchain -> bool
val get_version : toolchain -> string
val ready_toolchains : Workspace.workspace -> toolchain
val validate_toolchain : toolchain -> bool
val list_installed_toolchains : unit -> string list
