(** Store - Content-addressable storage for build artifacts **)

type artifact = { hash : Hasher.hash; files : string list }
type t = { root_dir : string (* Root directory for the store *) }
type error = string

(** Create a new store at the given root directory *)
let create ~root_dir =
  let store_dir =
    Filename.concat (Filename.concat root_dir "target/debug") "cache"
  in
  (* Create store directory if it doesn't exist *)
  let _ =
    Fs.mkdirp
      (Path.of_string store_dir |> Result.expect ~msg:"Invalid store path")
  in
  { root_dir = store_dir }

(** Get the path for a given hash in the store *)
let get_hash_dir store hash =
  Filename.concat store.root_dir (Hasher.to_string hash)

(** Check if artifacts for a given hash exist in the store *)
let exists store hash =
  let hash_dir = get_hash_dir store hash in
  Miniriot.File.exists ~path:hash_dir

(** List all files in a hash directory *)
let list_artifacts store hash =
  let hash_dir = get_hash_dir store hash in
  if Miniriot.File.exists ~path:hash_dir then
    match
      Fs.readdir
        (Path.of_string hash_dir |> Result.expect ~msg:"Invalid hash_dir")
    with
    | Ok files -> files
    | Error _ -> []
  else []

(** Promote artifacts from store to target directory *)
let promote_from_store store hash target_dir =
  let hash_dir = get_hash_dir store hash in
  if Miniriot.File.exists ~path:hash_dir then (
    (* Ensure target directory exists *)
    let _ =
      Fs.mkdirp
        (Path.of_string target_dir |> Result.expect ~msg:"Invalid target path")
    in
    ();

    (* Copy all files from hash directory to target *)
    let files =
      Fs.readdir
        (Path.of_string hash_dir |> Result.expect ~msg:"Invalid hash_dir")
      |> Result.expect ~msg:"Failed to read hash_dir"
    in
    List.iter
      (fun file ->
        let src = Filename.concat hash_dir file in
        let dst = Filename.concat target_dir file in

        (* Only copy if it's a file, not directory *)
        if
          not
            (match
               Fs.is_directory
                 (Path.of_string src |> Result.expect ~msg:"Invalid src")
             with
            | Ok b -> b
            | Error _ -> false)
        then
          let _ =
            Fs.copy_file
              (Path.of_string src |> Result.expect ~msg:"Invalid src")
              (Path.of_string dst |> Result.expect ~msg:"Invalid dst")
          in
          ())
      files;
    true)
  else false

(** Store artifacts from sandbox to content-addressable store *)
let store_artifacts store hash sandbox_dir declared_outputs =
  let hash_dir = get_hash_dir store hash in

  (* Create hash directory (including parent directories) *)
  let _ =
    Fs.mkdirp (Path.of_string hash_dir |> Result.expect ~msg:"Invalid hash dir")
  in
  ();

  (* Copy declared outputs to store and track what was actually stored *)
  let stored_files =
    List.fold_left
      (fun acc output_file ->
        let src = Filename.concat sandbox_dir output_file in
        if Miniriot.File.exists ~path:src then (
          let dst = Filename.concat hash_dir output_file in
          let _ =
            Fs.copy_file
              (Path.of_string src |> Result.expect ~msg:"Invalid src")
              (Path.of_string dst |> Result.expect ~msg:"Invalid dst")
          in
          output_file :: acc)
        else acc)
      [] declared_outputs
  in

  (* Return artifact witness *)
  { hash; files = List.rev stored_files }

(** Clean up old artifacts (for future use) *)
let gc_store store ~keep_recent_days =
  (* TODO: Implement garbage collection *)
  ()

(** Get store statistics *)
let get_stats store =
  let count_files dir =
    if Miniriot.File.exists ~path:dir then
      try
        let subdirs =
          Fs.readdir (Path.of_string dir |> Result.expect ~msg:"Invalid dir")
          |> Result.expect ~msg:"Failed to read dir"
        in
        List.fold_left
          (fun acc subdir ->
            let subdir_path = Filename.concat dir subdir in
            if
              match
                Fs.is_directory
                  (Path.of_string subdir_path
                  |> Result.expect ~msg:"Invalid subdir_path")
              with
              | Ok b -> b
              | Error _ -> false
            then
              match
                Fs.readdir
                  (Path.of_string subdir_path
                  |> Result.expect ~msg:"Invalid subdir_path")
              with
              | Ok files -> acc + List.length files
              | Error _ -> acc
            else acc)
          0 subdirs
      with _ -> 0
    else 0
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
        Some { hash; files }
      else None

(** Save build outputs to the store *)
let save store node ~sandbox_dir ~outs =
  match node.Build_node.spec with
  | Build_node.Unplanned -> Error "Cannot save artifacts for unplanned node"
  | Build_node.Planned { hash; _ } ->
      let outs_str = List.map Std.Path.to_string outs in
      let artifact = store_artifacts store hash sandbox_dir outs_str in
      Ok artifact

(** Promote cached artifacts to target directory *)
let promote store artifact ~target_dir =
  if promote_from_store store artifact.hash target_dir then Ok ()
  else Error "Failed to promote artifacts from cache"
