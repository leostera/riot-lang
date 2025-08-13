(** Build node definition - separated to avoid circular dependencies *)

type t = {
  package : Workspace.package;
  toolchain : Toolchains.toolchain;
  mutable dependencies : t list;
  mutable dependents : t list;
  mutable hash : Hasher.hash option; (* Content-based hash, computed on demand *)
}
