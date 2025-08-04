(** Build node definition - separated to avoid circular dependencies *)

type t = {
  package : Workspace.package;
  mutable dependencies : t list;
  mutable dependents : t list;
}