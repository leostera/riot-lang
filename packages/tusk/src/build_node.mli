(** Build node - Represents a package in the build dependency graph

    Each node contains a package and its relationships to other packages in the
    build graph, along with content-based hashing for caching. *)

type spec =
  | Unplanned  (** A node that hasn't been planned yet *)
  | Planned of {
      hash : Hasher.hash;
      outs : Path.t list;
      actions : Actions.action list;
    }  (** A node that has been through the planning stage *)

type t = {
  toolchain : Toolchains.toolchain;
  package : Workspace.package;
  srcs : Path.t list;
  mutable deps : Node_id.t list; (* Dependencies as node IDs *)
  mutable spec : spec;
}
(** A build node in the dependency graph *)

val is_planned : t -> bool
val is_unplanned : t -> bool
val compare : t -> t -> int

(** {1 Hash Computation} *)

(** Result type for hash computation *)
type hash_result =
  | Planned of t
  | MissingDependencies of { node : t; deps : t list }
  | Error of string

val compute_hash : t -> get_dep:(Node_id.t -> t option) -> hash_result
(** Force recomputation of hash for a node, ignoring any cached value. Returns
    the newly computed hash. *)
