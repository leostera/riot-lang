open Std
open Std.Collections
open Tusk_model

type t = {
  dir: Path.t;
  workspace: Workspace.t;
}

let sandbox_id = fun ~package_name ->
  let nanos = Time.SystemTime.duration_since_epoch () |> Time.Duration.to_nanos in
  let hash = Crypto.hash_int64 nanos in
  let hex = Crypto.Digest.hex hash in
  let truncated_hash = String.sub hex 0 8 in
  let id = package_name ^ "-" ^ truncated_hash in
  Path.v id

let create = fun ~workspace ?(profile = "debug") ?(target = Tusk_model.Tusk_dirs.host_target ()) ~package_name ->
  let sandbox_dir =
    Path.(Tusk_model.Tusk_dirs.sandbox_dir_with_target
    ~workspace_root:workspace.Workspace.root
    ~profile
    ~target
    / sandbox_id ~package_name) in
  Fs.create_dir_all sandbox_dir
  |> Result.expect ~msg:(((("Failed to create sandbox dir: " ^ (Path.to_string sandbox_dir)))));
  {dir = sandbox_dir; workspace}

let get_dir = fun t -> t.dir

let copy_object_files = fun ~store ~sandbox ~package ~depset ->
  let _ = store in
  let _ = package in
  let depset = Tusk_planner.Dependency.transitive_closure depset in
  List.iter
    (fun dep ->
      match Fs.read_dir dep.Tusk_planner.Dependency.artifact_dir with
      | Error _ -> ()
      | Ok reader ->
          let entries = Std.Iter.MutIterator.to_list reader in
          entries
          |> List.filter (fun path -> String.ends_with ~suffix:".o" (Path.to_string path))
          |> List.iter
            (fun entry ->
              let src =
                if Path.is_absolute entry then
                  entry
                else
                  Path.(dep.Tusk_planner.Dependency.artifact_dir / entry)
              in
              let dest = Path.(sandbox.dir / v (Path.basename entry)) in
              match Fs.copy ~src ~dst:dest with
              | Ok () -> ()
              | Error _ -> Log.warn
              ("Skipping unavailable dependency object file " ^ Path.to_string src)))
    depset

let copy_inputs = fun ~sandbox ~package ~inputs ->
  List.iter
    (fun rel_path ->
      let src = Path.(sandbox.workspace.Workspace.root / package.Package.relative_path / rel_path) in
      let dest = Path.(sandbox.dir / rel_path) in
      let dest_parent = Path.dirname dest in
      Fs.create_dir_all dest_parent
      |> Result.expect ~msg:(((("Failed to create parent dir: " ^ (Path.to_string dest_parent)))));
      Fs.copy ~src ~dst:dest
      |> Result.expect
      ~msg:(((("Failed to copy input " ^ Path.to_string src ^ " to " ^ (Path.to_string dest))))))
    inputs

let prepare = fun ~sandbox ~package ~inputs ~depset ~store ->
  (* No longer copy dependencies wholesale - dependencies are resolved via
     include directories in immutable cache paths, with only required object
     files copied into the sandbox for linker compatibility. *)
  copy_inputs ~sandbox ~package ~inputs;
  copy_object_files ~store ~sandbox ~package ~depset

let cleanup = fun sandbox ->
  let _ = Fs.remove_dir_all sandbox.dir in
  ()

let with_sandbox = fun ~workspace ?(profile = "debug") ?(target = Tusk_model.Tusk_dirs.host_target ()) ~package ~inputs ~depset ~store ~expected_outputs f ->
  let sandbox = create ~workspace ~profile ~target ~package_name:package.Package.name in
  let _ = expected_outputs in
  prepare ~sandbox ~package ~inputs ~depset ~store;
  let result = f sandbox in
  result
