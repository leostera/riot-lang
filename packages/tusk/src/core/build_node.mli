(** Build node - Represents a package in the build dependency graph

    Each node contains a package and its relationships to other packages in the
    build graph, along with content-based hashing for caching. *)
open Std
open Model


type spec =
  | Unplanned  (** A node that hasn't been planned yet *)
  | Planned of {
      hash : Std.Crypto.hash;
      outs : Path.t list;
      actions : Actions.action list;
    }  (** A node that has been through the planning stage *)

type source_kind = 
  | C_stub       (** .c stub file *)
  | ML of {      (** .ml implementation file *)
      simple_name: string;     (** Original module name (e.g., "Config") *)
      namespaced_name: string; (** Full namespaced name (e.g., "Std__Config") *)
      namespace: string list;  (** For future folder-based namespacing *)
    }
  | MLI of {     (** .mli interface file *)
      simple_name: string;     (** Original module name (e.g., "Config") *)
      namespaced_name: string; (** Full namespaced name (e.g., "Std__Config") *)
      namespace: string list;  (** For future folder-based namespacing *)
    }
  | Other of string  (** Other file types *)

type source = {
  file: Path.t;  (** The source file path *)
  kind: source_kind;  (** The type and metadata of source file *)
}
(** Source file with its type-specific metadata *)

type t = {
  toolchain : Toolchains.toolchain;
  package : Workspace.package;
  srcs : source list;
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
