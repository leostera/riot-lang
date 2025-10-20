open Std

(** Toolchain bootstrapping and management *)

type source = Version of string | Path of Path.t | Url of Net.Uri.t

module Ocamldep = Ocamldep
module Ocamlc = Ocamlc

type t
(** Toolchain type. Prefer using init() to get a validated instance. *)

val default_ocaml_version : string

val init : unit -> (t, string) result
(** Initialize and return the default OCaml toolchain.

    This checks if the toolchain binaries exist and are executable:
    - ocamlc.opt
    - ocamlopt.opt
    - ocamldep.opt

    If not found, attempts to use the ./ocaml/compiler directory if available,
    otherwise returns an error.

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

val hash : t -> Crypto.hash
(** Compute a hash of the toolchain for cache invalidation *)
