open Std
open Std.Iter

open Tusk_model
(** Store - Content-addressable storage for build artifacts **)

module Manifest = Manifest

type t = { root_dir : Path.t (* Root directory for the store *) }
type error = string

(** Create a new store for the given workspace *)
let create ~(workspace : Workspace.t) =
  let store_dir =
    Path.(workspace.root / Path.v "target" / Path.v "debug" / Path.v "cache")
  in
  Fs.create_dir_all store_dir
  |> Result.expect
       ~msg:
         (format "Failed to create store directory: %s"
            (Path.to_string store_dir));
  { root_dir = store_dir }

(** Get the path for a given hash in the store *)
let get_hash_dir store hash =
  Path.(store.root_dir / Path.v (Std.Crypto.Digest.hex hash))

(** Check if artifacts for a given hash exist in the store *)
let exists store hash =
  let hash_dir = get_hash_dir store hash in
  match Fs.exists hash_dir with Ok b -> b | Error _ -> false

(** Promote artifacts from store to target directory *)
let promote store hash ~target_dir =
  let hash_dir = get_hash_dir store hash in
  match Fs.exists hash_dir with
  | Ok true ->
      Fs.create_dir_all target_dir
      |> Result.expect
           ~msg:
             (format "Failed to create target directory: %s"
                (Path.to_string target_dir));

      let reader =
        Fs.read_dir hash_dir
        |> Result.expect
             ~msg:
               (format "Failed to read hash directory: %s"
                  (Path.to_string hash_dir))
      in
      let rec copy_files () =
        match MutIterator.next reader with
        | None -> ()
        | Some file_path -> (
            let file = Path.basename file_path in
            if String.equal file "manifest.json" then copy_files ()
            else
              let src = Path.(hash_dir / Path.v file) in
              let dst = Path.(target_dir / Path.v file) in
              match Fs.is_file src with
              | Ok true ->
                  Fs.copy ~src ~dst
                  |> Result.expect
                       ~msg:
                         (format "Failed to copy file: %s -> %s"
                            (Path.to_string src) (Path.to_string dst));
                  copy_files ()
              | Ok false -> copy_files ()
              | Error _ ->
                  panic
                    (format "Failed to check if %s is a file"
                       (Path.to_string src)))
      in
      copy_files ();
      Ok ()
  | Ok false -> Error "Hash not found in store"
  | Error _ ->
      panic
        (format "Failed to check if hash directory exists: %s"
           (Path.to_string hash_dir))

(** Store artifacts from sandbox to content-addressable store *)
let store_artifacts store ~package hash sandbox_dir declared_outputs =
  Log.debug "[Store] store_artifacts called for package %s" package;
  let hash_dir = get_hash_dir store hash in

  Fs.create_dir_all hash_dir
  |> Result.expect
       ~msg:
         (format "Failed to create hash directory: %s" (Path.to_string hash_dir));

  (* Copy declared outputs to store and track what was actually stored *)
  let stored_files_with_sizes =
    List.fold_left
      (fun acc output_file ->
        let src = Path.(sandbox_dir / Path.v output_file) in
        match Fs.exists src with
        | Ok true ->
            let dst = Path.(hash_dir / Path.v output_file) in
            Fs.copy ~src ~dst
            |> Result.expect
                 ~msg:
                   (format "Failed to store artifact: %s -> %s"
                      (Path.to_string src) (Path.to_string dst));
            let size =
              Fs.metadata dst
              |> Result.expect
                   ~msg:
                     (format "Failed to get metadata for %s"
                        (Path.to_string dst))
              |> Fs.Metadata.len
            in
            (dst, size) :: acc
        | _ -> acc)
      [] declared_outputs
  in

  (* Create and save manifest *)
  let manifest =
    Manifest.create ~package
      ~build_hash:(Std.Crypto.Digest.hex hash)
      ~files:(List.rev stored_files_with_sizes)
  in
  let manifest_path = Path.(hash_dir / Path.v "manifest.json") in
  Log.debug "[Store] Saving manifest to %s" (Path.to_string manifest_path);
  Manifest.save manifest ~path:manifest_path
  |> Result.expect ~msg:"Failed to save manifest";
  Log.debug "[Store] Manifest saved successfully";

  (* Return artifact witness with just the filenames *)
  let stored_files =
    List.map
      (fun (path, _) -> Path.v (Path.basename path))
      stored_files_with_sizes
  in
  Artifact.{ hash; files = List.rev stored_files }

(** Simple interface - check if we have cached artifacts for a hash *)
let get store hash =
  if exists store hash then
    let manifest_path =
      Path.(get_hash_dir store hash / Path.v "manifest.json")
    in
    match Manifest.load ~path:manifest_path with
    | Ok manifest ->
        let files =
          List.map (fun entry -> entry.Manifest.path) manifest.files
        in
        Some Artifact.{ hash; files }
    | Error _ -> None
  else None

(** Save build outputs to the store *)
let save store ~package ~hash ~sandbox_dir ~outs =
  let sandbox_str = Path.to_string sandbox_dir in
  let sandbox_len = String.length sandbox_str in
  let outs_str =
    List.map
      (fun out_path ->
        let out_str = Path.to_string out_path in
        if String.starts_with ~prefix:sandbox_str out_str then
          let relative_start = sandbox_len + 1 in
          Stdlib.String.sub out_str relative_start
            (String.length out_str - relative_start)
        else Path.basename out_path)
      outs
  in
  let artifact = store_artifacts store ~package hash sandbox_dir outs_str in
  Ok artifact

(** Promote cached artifacts to target directory *)
let promote_artifact store artifact ~target_dir =
  promote store Artifact.(artifact.hash) target_dir
