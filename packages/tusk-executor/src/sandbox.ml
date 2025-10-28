open Std
open Tusk_model

type t = { dir : Path.t; workspace : Workspace.t }

let sandbox_id ~package_name =
  let nanos =
    Time.SystemTime.duration_since_epoch () |> Time.Duration.to_nanos
  in
  let hash = Crypto.hash_int64 nanos in
  let hex = Crypto.Digest.hex hash in
  let truncated_hash = String.sub hex 0 8 in
  let id = format "%s-%s" package_name truncated_hash in
  Path.v id

let create ~workspace ~package_name =
  let sandbox_dir =
    Path.(
      workspace.Workspace.root / v "target" / v "debug" / v "sandbox"
      / sandbox_id ~package_name)
  in
  Fs.create_dir_all sandbox_dir
  |> Result.expect
       ~msg:
         (format "Failed to create sandbox dir: %s"
            (Path.to_string sandbox_dir));
  { dir = sandbox_dir; workspace }

let get_dir t = t.dir

let copy_object_files ~store ~sandbox ~package ~depset =
  List.iter
    (fun dep ->
      let abs_paths =
        Tusk_store.Store.get_artifact_paths store
          dep.Tusk_planner.Dependency.artifact
      in
      let o_files =
        List.filter
          (fun path -> String.ends_with ~suffix:".o" (Path.to_string path))
          abs_paths
      in
      List.iter
        (fun abs_path ->
          let dest = Path.(sandbox.dir / v (Path.basename abs_path)) in
          Fs.copy ~src:abs_path ~dst:dest
          |> Result.expect
               ~msg:
                 (format "Failed to copy .o file %s" (Path.to_string abs_path)))
        o_files)
    depset

let copy_inputs ~sandbox ~package ~inputs =
  List.iter
    (fun rel_path ->
      let src =
        Path.(
          sandbox.workspace.Workspace.root / package.Package.relative_path
          / rel_path)
      in
      let dest = Path.(sandbox.dir / rel_path) in
      let dest_parent = Path.dirname dest in
      Fs.create_dir_all dest_parent
      |> Result.expect
           ~msg:
             (format "Failed to create parent dir: %s"
                (Path.to_string dest_parent));
      Fs.copy ~src ~dst:dest
      |> Result.expect
           ~msg:
             (format "Failed to copy input %s to %s" (Path.to_string src)
                (Path.to_string dest)))
    inputs

let cleanup sandbox =
  let _ = Fs.remove_dir_all sandbox.dir in
  ()

let with_sandbox ~workspace ~package ~inputs ~depset ~store ~expected_outputs f
    =
  let sandbox = create ~workspace ~package_name:package.Package.name in
  (* No longer copy dependencies - they're accessed via -I flags pointing to cache *)
  copy_inputs ~sandbox ~package ~inputs;
  copy_object_files ~store ~sandbox ~package ~depset;
  let result = f sandbox in
  result
