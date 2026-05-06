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

type dependency_copy_error =
  | DependencyArtifactDirUnavailable of {
      package: Package_name.t;
      artifact_dir: Path.t;
      message: string;
    }
  | DependencyObjectCopyFailed of {
      package: Package_name.t;
      src: Path.t;
      dst: Path.t;
      message: string;
    }

type prepare_stats = {
  input_count: int;
  dependency_count: int;
  dependency_object_count: int;
}

type prepare_error =
  | InputCopyFailed of { message: string }
  | DependencyCopyFailed of dependency_copy_error

let dependency_copy_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | DependencyArtifactDirUnavailable { package; artifact_dir; message } ->
      "Dependency artifact directory for "
      ^ Package_name.to_string package
      ^ " is unavailable at "
      ^ Path.to_string artifact_dir
      ^ ": "
      ^ message
  | DependencyObjectCopyFailed { package; src; dst; message } ->
      "Failed to copy dependency object for "
      ^ Package_name.to_string package
      ^ " from "
      ^ Path.to_string src
      ^ " to "
      ^ Path.to_string dst
      ^ ": "
      ^ message

let prepare_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | InputCopyFailed { message } -> "Failed to copy package inputs: " ^ message
  | DependencyCopyFailed err -> dependency_copy_error_to_string err

let short_hash = fun hash ->
  Crypto.Digest.hex hash
  |> fun hex -> String.sub hex ~offset:0 ~len:16

let default_sandbox_seed = fun ~package_name ->
  let pid =
    Process.id ()
    |> Int32.to_string
  in
  let nanos =
    Time.SystemTime.duration_since_epoch ()
    |> Time.Duration.to_nanos
    |> Int64.to_string
  in
  Crypto.hash_string (Package_name.to_string package_name ^ ":" ^ pid ^ ":" ^ nanos)

let sandbox_id = fun ~id_seed ~session_id ~package_name ->
  let seed =
    match id_seed with
    | Some input_hash ->
        let session =
          session_id
          |> Option.map ~fn:Session_id.to_string
          |> Option.unwrap_or ~default:""
        in
        Crypto.hash_string (Crypto.Digest.hex input_hash ^ ":" ^ session)
    | None -> default_sandbox_seed ~package_name
  in
  Path.v (Package_name.to_string package_name ^ "-" ^ short_hash seed)

let absolute_path = fun path ->
  if Path.is_absolute path then
    Path.normalize path
  else
    match Env.current_dir () with
    | Ok cwd -> Path.normalize Path.(cwd / path)
    | Error _ -> Path.normalize path

let create = fun
  ~workspace
  ?id_seed
  ?session_id
  ?(profile = "debug")
  ?(target = Riot_model.Riot_dirs.host_target ())
  ()
  ~package_name ->
  let sandbox_dir =
    Path.(Riot_model.Riot_dirs.sandbox_dir_in_workspace ~workspace ~profile ~target
    / sandbox_id ~id_seed ~session_id ~package_name)
    |> absolute_path
  in
  Fs.create_dir_all sandbox_dir
  |> Result.expect ~msg:("Failed to create sandbox dir: " ^ (Path.to_string sandbox_dir));
  { dir = sandbox_dir; workspace }

let get_dir = fun t -> t.dir

let copy_dependency_object_files = fun ~store ~sandbox ~package ~depset ->
  let _ = package in
  let depset = Riot_planner.Dependency.transitive_closure depset in
  let copy_dependency = fun copied dep ->
    let dep_package = dep.Riot_planner.Dependency.package.Package.name in
    let artifact_dir = dep.Riot_planner.Dependency.artifact_dir in
    match Riot_store.Store.get_package store dep.Riot_planner.Dependency.input_hash with
    | None ->
        Error
          (
            DependencyArtifactDirUnavailable {
              package = dep_package;
              artifact_dir;
              message = "package artifact manifest is missing or incomplete";
            }
          )
    | Some artifact ->
        let entries = artifact.Riot_store.Artifact.files in
        let copy_entry = fun copied (entry: Riot_store.Manifest.file_entry) ->
          let entry_path = entry.Riot_store.Manifest.path in
          if String.ends_with ~suffix:".o" (Path.to_string entry_path) then (
            let src = Path.(artifact_dir / entry_path) in
            let dst = Path.(sandbox.dir / v (Path.basename entry_path)) in
            match Fs.copy ~src ~dst with
            | Ok () -> Ok (copied + 1)
            | Error err ->
                Error
                  (
                    DependencyObjectCopyFailed {
                      package = dep_package;
                      src;
                      dst;
                      message = IO.error_message err;
                    }
                  )
          ) else
            Ok copied
        in
        List.fold_left
          entries
          ~init:(Ok copied)
          ~fn:(fun result entry ->
            match result with
            | Error _ as err -> err
            | Ok copied -> copy_entry copied entry)
  in
  match
    List.fold_left
      depset
      ~init:(Ok 0)
      ~fn:(fun result dep ->
        match result with
        | Error _ as err -> err
        | Ok copied -> copy_dependency copied dep)
  with
  | Error _ as err -> err
  | Ok object_count -> Ok { dependency_count = List.length depset; object_count }

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
  match copy_inputs ~sandbox ~package ~inputs with
  | exception exn -> Error (InputCopyFailed { message = Exception.to_string exn })
  | input_count -> (
      match copy_dependency_object_files ~store ~sandbox ~package ~depset with
      | Error err -> Error (DependencyCopyFailed err)
      | Ok dependency_stats ->
          Ok {
            input_count;
            dependency_count = dependency_stats.dependency_count;
            dependency_object_count = dependency_stats.object_count;
          }
    )

let cleanup = fun sandbox ->
  let _ = Fs.remove_dir_all sandbox.dir in
  ()

let with_sandbox = fun
  ~workspace
  ?id_seed
  ?session_id
  ?(profile = "debug")
  ?(target = Riot_model.Riot_dirs.host_target ())
  ~package
  ~inputs
  ~depset
  ~store
  ~expected_outputs
  f ->
  let sandbox =
    create
      ~workspace
      ?id_seed
      ?session_id
      ~profile
      ~target
      ()
      ~package_name:package.Package.name
  in
  let _ = expected_outputs in
  match prepare ~sandbox ~package ~inputs ~depset ~store with
  | Error err -> panic (prepare_error_to_string err)
  | Ok _ ->
      let result = f sandbox in
      result
