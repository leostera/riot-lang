(** Build node - Represents a package in the build dependency graph
    
    Each node contains a package and its relationships to other packages
    in the build graph, along with content-based hashing for caching. *)

(** A build node in the dependency graph *)
type t = {
  package : Workspace.package;
  (** The package this node represents *)

  toolchain : Toolchains.toolchain;
  (** OCaml toolchain to use *)
  
  mutable dependencies : t list;
  (** Packages this node depends on (must be built first) *)
  
  mutable dependents : t list;
  (** Packages that depend on this node *)
  
  mutable hash : Hasher.hash option;
  (** Content-based hash for caching, computed on demand.
      [None] means the hash hasn't been computed yet. *)
}
