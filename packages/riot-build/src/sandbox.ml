open Std
open Std.Collections
open Riot_model

type t = {
  dir: Path.t;
  workspace: Workspace.t;
}

type dependency_copy_stats = {
  dependency_count: int;
  object_count: int;
}

type prepare_stats = {
  input_count: int;
  dependency_count: int;
  dependency_object_count: int;
}

let sandbox_id = fun ~package_name ->
  let nanos =
    Time.SystemTime.duration_since_epoch ()
    |> Time.Duration.to_nanos
  in
  let hash = Crypto.hash_int64 nanos in
  let hex = Crypto.Digest.hex hash in
  let truncated_hash = String.sub hex ~offset:0 ~len:8 in
  let id = Package_name.to_string package_name ^ "-" ^ truncated_hash in
  Path.v id

let absolute_path = fun path ->
  if Path.is_absolute path then
    Path.normalize path
  else
    match Env.current_dir () with
    | Ok cwd -> Path.normalize Path.(cwd / path)
    | Error _ -> Path.normalize path

let create = fun
  ~workspace
  ?(profile = "debug")
  ?(target = Riot_model.Riot_dirs.host_target ())
  ()
  ~package_name ->
  let sandbox_dir =
    Path.(Riot_model.Riot_dirs.sandbox_dir_in_workspace ~workspace ~profile ~target
    / sandbox_id ~package_name)
    |> absolute_path
  in
  Fs.create_dir_all sandbox_dir
  |> Result.expect ~msg:("Failed to create sandbox dir: " ^ (Path.to_string sandbox_dir));
  { dir = sandbox_dir; workspace }

let get_dir = fun t -> t.dir

let copy_dependency_object_files = fun ~store ~sandbox ~package ~depset ->
  let _ = store in
  let _ = package in
  let depset = Riot_planner.Dependency.transitive_closure depset in
  let object_count =
    List.fold_left
    depset
    ~init:0
    ~fn:(fun copied dep ->
      match Fs.read_dir dep.Riot_planner.Dependency.artifact_dir with
      | Error _ -> copied
      | Ok reader ->
          let entries = Std.Iter.MutIterator.to_list reader in
          entries
          |> List.fold_left
            ~init:copied
            ~fn:(fun copied entry ->
              if String.ends_with ~suffix:".o" (Path.to_string entry) then (
                let src =
                  if Path.is_absolute entry then
                    entry
                  else
                    Path.(dep.Riot_planner.Dependency.artifact_dir / entry)
                in
                let dest = Path.(sandbox.dir / v (Path.basename entry)) in
                match Fs.copy ~src ~dst:dest with
                | Ok () -> copied + 1
                | Error _ ->
                    Log.warn ("Skipping unavailable dependency object file " ^ Path.to_string src);
                    copied
              ) else
                copied))
  in
  { dependency_count = List.length depset; object_count }

let copy_inputs = fun ~sandbox ~package ~inputs ->
  List.for_each
    inputs
    ~fn:(fun rel_path ->
      let src =
        Path.(sandbox.workspace.Workspace.root / package.Package.relative_path / rel_path)
      in
      let dest = Path.(sandbox.dir / rel_path) in
      let dest_parent = Path.dirname dest in
      Fs.create_dir_all dest_parent
      |> Result.expect ~msg:("Failed to create parent dir: " ^ (Path.to_string dest_parent));
      Fs.copy ~src ~dst:dest
      |> Result.expect
        ~msg:("Failed to copy input " ^ Path.to_string src ^ " to " ^ (Path.to_string dest)));
  List.length inputs

let prepare = fun ~sandbox ~package ~inputs ~depset ~store ->
  (* No longer copy dependencies wholesale - dependencies are resolved via
     include directories in immutable cache paths, with only required object
     files copied into the sandbox for linker compatibility.
  *)
  let input_count = copy_inputs ~sandbox ~package ~inputs in
  let dependency_stats = copy_dependency_object_files ~store ~sandbox ~package ~depset in
  {
    input_count;
    dependency_count = dependency_stats.dependency_count;
    dependency_object_count = dependency_stats.object_count;
  }

let cleanup = fun sandbox ->
  let _ = Fs.remove_dir_all sandbox.dir in
  ()

let with_sandbox = fun
  ~workspace
  ?(profile = "debug")
  ?(target = Riot_model.Riot_dirs.host_target ())
  ~package
  ~inputs
  ~depset
  ~store
  ~expected_outputs
  f ->
  let sandbox = create ~workspace ~profile ~target () ~package_name:package.Package.name in
  let _ = expected_outputs in
  let _ = prepare ~sandbox ~package ~inputs ~depset ~store in
  let result = f sandbox in
  result
