(** Client and cache helpers for the `pkgs.ml` package registry. *)

(** Local on-disk cache layout for a registry. *)
module Registry_cache = Registry_cache

(** Registry HTTP client helpers. *)
module Registry = Registry

(** Sparse-index helpers and data structures. *)
module Sparse_index = Sparse_index
