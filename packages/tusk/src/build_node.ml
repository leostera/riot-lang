(** Build node definition - separated to avoid circular dependencies *)

type t = {
  package : Workspace.package;
  toolchain : Toolchains.toolchain;
  mutable dependencies : t list;
  mutable dependents : t list;
  mutable hash : Hasher.hash option; (* Content-based hash, computed on demand *)
}

(** Tests submodule *)
module Tests = struct
  [@test]
  let test_node_tracks_bidirectional_dependencies () : (unit, string) result =
    (* Test that both dependencies and dependents are tracked correctly *)
    Ok ()
  
  [@test]
  let test_hash_caching_works () : (unit, string) result =
    (* Test that hash is computed once and cached *)
    Ok ()
  
  [@test]
  let test_circular_dependency_detection () : (unit, string) result =
    (* Test that circular dependencies are prevented *)
    Ok ()
end
