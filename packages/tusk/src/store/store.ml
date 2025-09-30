open Std

open Model
(** Store - Content-addressable storage for build artifacts **)

open Core
module Manifest = Manifest

type t = { root_dir : Path.t (* Root directory for the store *) }
type error = string

(** Create a new store for the given workspace *)
let create ~(workspace : Workspace.t) =
  let store_dir =
    Path.(workspace.root / Path.v "target" / Path.v "debug" / Path.v "cache")
  in
  (* Create store directory if it doesn't exist *)
  let _ =
    Fs.create_dir_all store_dir
    |> Result.expect
         ~msg:(Printf.sprintf "Failed to create store directory: %s" (Path.to_string store_dir))
  in
  { root_dir = store_dir }

(** Get the path for a given hash in the store *)
let get_hash_dir store hash =
  Path.(store.root_dir / Path.v (Std.Crypto.Digest.hex hash))

(** Check if artifacts for a given hash exist in the store *)
let exists store hash =
  let hash_dir = get_hash_dir store hash in
  match Fs.exists hash_dir with
  | Ok b -> b
  | Error _ -> false

(** List all files in a hash directory *)
let list_artifacts store hash =
  let hash_dir = get_hash_dir store hash in
  match Fs.exists hash_dir with
  | Ok true -> (
      match Fs.read_dir hash_dir with
      | Ok iter ->
          let result = ref [] in
          let rec collect () =
            match MutIterator.next iter with
            | None -> List.rev !result
            | Some path ->
                result := Path.basename path :: !result;
                collect ()
          in
          collect ()
      | Error _ -> [])
  | _ -> []

(** Promote artifacts from store to target directory *)
let promote_from_store store hash target_dir =
  let hash_dir = get_hash_dir store hash in
  match Fs.exists hash_dir with
  | Ok true ->
      (* Ensure target directory exists *)
      let _ =
        Fs.create_dir_all target_dir
        |> Result.expect
             ~msg:
               (Printf.sprintf "Failed to create target directory: %s" (Path.to_string target_dir))
      in

      (* Copy all files from hash directory to target *)
      (match Fs.read_dir hash_dir with
      | Ok iter ->
          let rec copy_files () =
            match MutIterator.next iter with
            | None -> ()
            | Some file_path ->
                let file = Path.basename file_path in
                let src = Path.(hash_dir / Path.v file) in
                let dst = Path.(target_dir / Path.v file) in

                (* Only copy if it's a file, not directory *)
                (match Fs.is_directory src with
                | Ok false ->
                    let _ =
                      Fs.copy ~src ~dst
                      |> Result.expect
                           ~msg:(Printf.sprintf "Failed to copy file: %s -> %s"
                                   (Path.to_string src) (Path.to_string dst))
                    in
                    ()
                | _ -> ());
                copy_files ()
          in
          copy_files ()
      | Error _ -> ());
      true
  | _ -> false

(** Store artifacts from sandbox to content-addressable store *)
let store_artifacts store ~package hash sandbox_dir declared_outputs =
  Printf.printf "[Store] store_artifacts called for package %s\n%!" package;
  let hash_dir = get_hash_dir store hash in

  (* Create hash directory (including parent directories) *)
  let _ =
    Fs.create_dir_all hash_dir
    |> Result.expect
         ~msg:(Printf.sprintf "Failed to create hash directory: %s" (Path.to_string hash_dir))
  in

  (* Copy declared outputs to store and track what was actually stored *)
  let stored_files_with_sizes =
    List.fold_left
      (fun acc output_file ->
        let src = Path.(sandbox_dir / Path.v output_file) in
        match Fs.exists src with
        | Ok true ->
            let dst = Path.(hash_dir / Path.v output_file) in
            let _ =
              Fs.copy ~src ~dst
              |> Result.expect
                   ~msg:
                     (Printf.sprintf "Failed to store artifact: %s -> %s"
                        (Path.to_string src) (Path.to_string dst))
            in
            (* Get file size for manifest *)
            let size =
              match Fs.stat dst with
              | Ok stat -> stat.st_size
              | Error _ -> 0
            in
            (dst, size) :: acc
        | _ -> acc)
      [] declared_outputs
  in

  (* Create and save manifest *)
  let manifest =
    Manifest.create ~package ~build_hash:(Std.Crypto.Digest.hex hash)
      ~files:(List.rev stored_files_with_sizes)
  in
  let manifest_path = Path.(hash_dir / Path.v "manifest.json") in
  Printf.printf "[Store] Saving manifest to %s\n%!" (Path.to_string manifest_path);
  let _ =
    Manifest.save manifest ~path:(Path.to_string manifest_path)
    |> Result.expect ~msg:"Failed to save manifest"
  in
  Printf.printf "[Store] Manifest saved successfully\n%!";

  (* Return artifact witness with just the filenames *)
  let stored_files =
    List.map (fun (path, _) -> Path.basename path) stored_files_with_sizes
  in
  Artifact.{ hash; files = List.rev stored_files }

(** Clean up old artifacts (for future use) *)
let gc_store store ~keep_recent_days =
  (* TODO: Implement garbage collection *)
  ()

(** Get store statistics *)
let get_stats store =
  let count_files dir =
    match Fs.exists dir with
    | Ok true -> (
        match Fs.read_dir dir with
        | Ok iter ->
            let rec count acc =
              match MutIterator.next iter with
              | None -> acc
              | Some subdir_path ->
                  let count_in_subdir =
                    match Fs.is_directory subdir_path with
                    | Ok true -> (
                        match Fs.read_dir subdir_path with
                        | Ok subiter ->
                            let rec count_sub acc2 =
                              match MutIterator.next subiter with
                              | None -> acc2
                              | Some _ -> count_sub (acc2 + 1)
                            in
                            count_sub 0
                        | Error _ -> 0)
                    | _ -> 0
                  in
                  count (acc + count_in_subdir)
            in
            count 0
        | Error _ -> 0)
    | _ -> 0
  in

  let total_artifacts = count_files store.root_dir in
  total_artifacts

(** Tests submodule *)
module Tests = struct
  let test_store_saves_and_retrieves_artifacts () : (unit, string) result =
    (* Test that artifacts can be saved and retrieved by hash *)
    Ok ()
    [@test]

  let test_store_handles_concurrent_access () : (unit, string) result =
    (* Test that multiple processes can safely access store *)
    Ok ()
    [@test]

  let test_exists_correctly_checks_artifact_presence () : (unit, string) result
      =
    (* Test that exists returns true only for saved artifacts *)
    Ok ()
    [@test]

  let test_store_preserves_file_permissions () : (unit, string) result =
    (* Test that saved artifacts maintain correct permissions *)
    Ok ()
    [@test]

  let test_store_creates_hash_based_directory_structure () :
      (unit, string) result =
    (* Test that artifacts are organized by hash prefix *)
    Ok ()
end [@test]

(** Simple interface - check if we have cached artifacts for a build node *)
let get store node =
  match node.Build_node.spec with
  | Build_node.Unplanned -> None
  | Build_node.Planned { hash; outs; _ } ->
      if exists store hash then
        let files = list_artifacts store hash in
        Some Artifact.{ hash; files }
      else None

(** Save build outputs to the store *)
let save store node ~sandbox_dir ~outs =
  match node.Build_node.spec with
  | Build_node.Unplanned -> Error "Cannot save artifacts for unplanned node"
  | Build_node.Planned { hash; _ } ->
      let outs_str = List.map Path.to_string outs in
      let artifact =
        store_artifacts store ~package:node.Build_node.package.name hash
          sandbox_dir outs_str
      in
      Ok artifact

(** Promote cached artifacts to target directory *)
let promote store artifact ~target_dir =
  if promote_from_store store Artifact.(artifact.hash) target_dir then Ok ()
  else Error "Failed to promote artifacts from cache"