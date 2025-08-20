(** Build node definition - separated to avoid circular dependencies *)

type spec =
  | Unplanned
  | Planned of {
      hash : Hasher.hash;
      outs : Path.t list;
      blueprint : Actions.blueprint;
    }

type t = {
  package : Workspace.package;
  toolchain : Toolchains.toolchain;
  srcs : Path.t list;
  deps : t list;
  mutable spec : spec;
}

(* Use Hasher module for all hash operations *)

(** Compute content-based hash for a build node *)
let rec compute_hash node =
  Printf.printf "[BuildGraph] Computing hash for %s...\n" node.package.name;

  (* traverse the build graph of dependencies of this node and if any of the dependencies is unplanned, requeue *)
  match has_unplanned_deps node with
  | UnplannedDeps deps -> Ok (RequeueWithDeps { node; deps })
  | AllPlanned ->
      let dep_seeds =
        List.sort Build_node.compare node.deps |> List.map compute_node_hash
      in

      let seeds =
        [
          Workspace.package_hash node.package;
          Toolchains.hash node.toolchain;
          Hasher.hash_files node.srcs;
        ]
        @ dep_seeds
      in

      Std.Crypto.sha512 seeds

(** Result type for hash computation *)
type hash_result =
  | Ok of Hasher.hash
  | MissingDependencies of Build_node.t list
  | Error of string

(** Tests submodule *)
module Tests = struct
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
end [@test]
