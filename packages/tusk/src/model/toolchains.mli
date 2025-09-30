type toolchain

val default_ocaml_version : string
val default_toolchain : toolchain
val get_toolchain_path : toolchain -> Std.Path.t
val get_version : toolchain -> string
val is_toolchain_installed : toolchain -> bool
val list_installed_toolchains : unit -> string list
val ocamlc_path : toolchain -> Std.Path.t
val ocamldep_path : toolchain -> Std.Path.t
val ocamlopt_path : toolchain -> Std.Path.t
val ocamlformat_path : toolchain -> Std.Path.t
val ready_toolchains : Workspace.t -> toolchain
val toolchain_base_dir : Std.Path.t
val validate_toolchain : toolchain -> bool

val hash : toolchain -> Std.Crypto.hash
(** Hash a toolchain - hashes the compiler binary *)
