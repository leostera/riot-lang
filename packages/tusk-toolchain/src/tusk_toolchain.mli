open Std

(** Toolchain bootstrapping and management *)

type source = Version of string | Path of Path.t | Url of Net.Uri.t

module Ocamldep = Ocamldep
module Ocamlc = Ocamlc
module Ocamlformat = Ocamlformat

type t
(** Toolchain type. Prefer using init() to get a validated instance. *)

val default_ocaml_version : string

val init : config:Tusk_model.Toolchain_config.t -> (t, string) result
(** Initialize and return the OCaml toolchain for the given config.

    This: 1. Uses the provided toolchain config 2. Checks if the toolchain
    binaries exist and are executable 3. If not found, attempts to use
    ./ocaml/compiler directory if available

    Returns Ok toolchain if ready, Error msg otherwise. *)

val ensure_default_toolchain : unit -> (unit, string) result
(** Ensure the default toolchain is bootstrapped without returning it. Useful
    for setup code that just needs to verify toolchain exists. *)

val check_health : t -> (unit, string) result
(** Check if a toolchain is properly installed and functional.

    Verifies:
    - All required binaries exist and are executable
    - Binaries can be executed (basic --version check)

    Returns Ok () if healthy, Error msg with details otherwise. *)

(** Access toolchain components *)

val ocamlc : t -> Ocamlc.t
val ocamlopt_path : t -> Path.t
val ocamldep : t -> Ocamldep.t
val ocamlformat : t -> Ocamlformat.t

val hash : t -> Crypto.hash
(** Compute a hash of the toolchain for cache invalidation *)
