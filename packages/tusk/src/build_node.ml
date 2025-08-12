(** Build node definition - separated to avoid circular dependencies *)

type t = {
  package : Workspace.package;
  mutable dependencies : t list;
  mutable dependents : t list;
  mutable hash : Hasher.hash option; (* Content-based hash, computed on demand *)
}
