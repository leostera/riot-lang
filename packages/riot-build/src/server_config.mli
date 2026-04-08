open Std

(** Configuration for the local [riot-build] runtime.

    This package no longer models a long-lived daemon, but some internal
    runtime code still needs a small typed configuration surface.
*)
type t

(** Default local build-runtime configuration. *)
val default: t

(** Compare two configurations for semantic equality. *)
val equal: t -> t -> bool
