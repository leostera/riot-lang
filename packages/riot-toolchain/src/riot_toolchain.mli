open Std

(** Toolchain bootstrapping and management *)
type source =
  | Version of string
  | Path of Path.t
  | Url of Net.Uri.t

module Ocamldep = Ocamldep

module Ocamlc = Ocamlc

module CrossCompilingToolchain = Cross_compiling_toolchain

type t

(** Toolchain type. Prefer using init() to get a validated instance. *)
val default_ocaml_version: string

val init: config:Riot_model.Toolchain_config.t -> (t, string) result

(**
   Initialize and return the OCaml toolchain for the given config.

   This: 1. Uses the provided toolchain config 2. Checks if the toolchain
   binaries exist and are executable 3. If not found, attempts to use
   ./vendor/ocaml/compiler directory if available

   Returns Ok toolchain if ready, Error msg otherwise.
*)
val ensure_default_toolchain: unit -> (unit, string) result

(**
   Ensure the default toolchain is bootstrapped without returning it. Useful
   for setup code that just needs to verify toolchain exists.
*)
val check_health: t -> (unit, string) result

(**
   Check if a toolchain is properly installed and functional.

   Verifies:
   - All required binaries exist and are executable
   - Binaries can be executed (basic --version check)

   Returns Ok () if healthy, Error msg with details otherwise.
*)

(** Access toolchain components *)
val ocamlc: t -> Ocamlc.t

val ocamlc_bytecode: t -> Ocamlc.t

val ocamlopt_path: t -> Path.t

val ocamldep: t -> Ocamldep.t

val path: t -> Path.t

val c_compiler: t -> Path.t option

val hash: t -> Crypto.hash

(** Compute a hash of the toolchain for cache invalidation *)

(** Multi-target toolchain support *)
val get_host_triple: unit -> Riot_model.Target.t

(** Get the current host architecture triple *)

(**
   Build the expected toolchain identity for a target without validating,
   downloading, or otherwise readying it.
*)
val from_config_for_target: config:Riot_model.Toolchain_config.t -> target:Riot_model.Target.t -> t

val init_for_target:
  config:Riot_model.Toolchain_config.t ->
  target:Riot_model.Target.t ->
  (t, string) result

(**
   Initialize toolchain for a specific target architecture.

   Supports both native and cross-compilation toolchains:
   - If target == host: Uses native toolchain (same as init)
   - If target != host: Downloads and uses cross-compilation toolchain

   Returns Ok toolchain if ready, Error msg otherwise.
*)
val get_for_target:
  config:Riot_model.Toolchain_config.t ->
  target:Riot_model.Target.t ->
  (t, string) result

(**
   Get toolchain for specific target (lazy initialization).
   Equivalent to init_for_target but more explicit about intent.
*)
val download_and_install_toolchain:
  string ->
  host:Riot_model.Target.t ->
  target:Riot_model.Target.t ->
  (unit, string) result

(**
   Download and install a toolchain for the given version and target.
   Returns Ok () on success, Error msg on failure.
*)

(** Toolchain management *)
type toolchain_status =
  | Installed of {
      path: Path.t;
    }
  | NotInstalled of {
      expected_path: Path.t;
    }
  | Incomplete of {
      path: Path.t;
      missing: string list;
    }
type toolchain_info = {
  version: string;
  target: Riot_model.Target.t;
  is_host: bool;
  status: toolchain_status;
}
type available_toolchain_kind =
  | Native
  | Cross
type available_toolchain = {
  version: string;
  host: Riot_model.Target.t;
  target: Riot_model.Target.t;
  artifact_target: string;
  kind: available_toolchain_kind;
  artifact: string;
  artifact_url: string;
  checksum_url: string;
  size_bytes: int option;
  last_modified: string option;
}

val list_toolchains: config:Riot_model.Toolchain_config.t -> toolchain_info list

(** List all toolchains configured for this project with their status *)
val check_toolchain_status: version:string -> target:Riot_model.Target.t -> toolchain_status

(** Check the installation status of a specific toolchain *)
val install_all_toolchains: config:Riot_model.Toolchain_config.t -> (int * int, string) result

(**
   Install all missing toolchains from config.
   Returns Ok (installed_count, skipped_count) or Error msg.
   Prints progress during installation.
*)
val list_available_toolchains: unit -> (available_toolchain list, string) result

(** Fetch and parse the published OCaml toolchain manifest. *)
