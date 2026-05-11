open Std
open Std.Result.Syntax

module ConcurrentHashMap = Collections.ConcurrentHashMap

type prepared = {
  build: Goal.build_package;
  package: Riot_model.Package.t;
  root: Path.t;
}

type t = {
  workspace: Riot_model.Workspace.t;
  store: Riot_store.Store.t;
  sandboxes: (Goal.build_package, prepared) ConcurrentHashMap.t;
}

let create = fun ~workspace ~store () -> {
  workspace;
  store;
  sandboxes = ConcurrentHashMap.with_capacity ~size:128;
}

let begin_execution = fun t -> ConcurrentHashMap.clear t.sandboxes

let check_dir = Path.v ".store/check"

let link_dir = Path.v ".store/link"

let deps_dir = fun package ->
  Path.(Path.v ".deps" / Path.v (Riot_model.Package_name.to_string package))

let dep_check_dir = fun package -> Path.(deps_dir package / check_dir)

let dep_link_dir = fun package -> Path.(deps_dir package / link_dir)

let path_error_message = fun __tmp1 ->
  match __tmp1 with
  | Path.InvalidUtf8 { path } -> "invalid utf8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } -> syscall ^ " returned invalid utf8 path: " ^ path
  | Path.SystemError message -> message

let absolute_path = fun path ->
  if Path.is_absolute path then
    Ok path
  else
    Env.current_dir ()
    |> Result.map ~fn:(fun cwd -> Path.normalize Path.(cwd / path))
    |> Result.map_err
      ~fn:(fun error ->
        Error.ExecutorInvariantViolated {
          message = "failed to resolve current directory: " ^ path_error_message error;
        })

let store_error = fun ?package reason -> Error.StoreFailed { package; reason }

let fs_error = fun action path error ->
  Error.ExecutorInvariantViolated {
    message = action ^ " " ^ Path.to_string path ^ ": " ^ IO.error_message error;
  }

let ensure_dir = fun path ->
  Fs.create_dir_all path
  |> Result.map_err ~fn:(fs_error "failed to create package sandbox directory" path)

let ensure_parent_dir = fun path ->
  match Path.parent path with
  | Some parent -> ensure_dir parent
  | None -> Ok ()

let copy_file = fun ~src ~dst ->
  let* () = ensure_parent_dir dst in
  Fs.copy ~src ~dst
  |> Result.map_err ~fn:(fs_error "failed to copy package sandbox input" src)

let package_relative_path = fun (package: Riot_model.Package.t) path ->
  if Path.is_absolute path then
    Path.strip_prefix (Path.normalize path) ~prefix:(Path.normalize package.path)
    |> Result.map_err
      ~fn:(fun _ ->
        Error.ExecutorInvariantViolated {
          message = "package sandbox input path is outside package root: " ^ Path.to_string path;
        })
  else
    Ok path

let copy_package_file = fun (package: Riot_model.Package.t) ~root path ->
  let* relative = package_relative_path package path in
  let src = Path.(package.path / relative) in
  let dst = Path.(root / relative) in
  match Fs.exists src with
  | Ok true -> copy_file ~src ~dst
  | Ok false when Path.equal relative (Path.v "riot.toml") -> Ok ()
  | Ok false ->
      Error (Error.ExecutorInvariantViolated {
        message = "package sandbox input is missing: " ^ Path.to_string src;
      })
  | Error error -> Error (fs_error "failed to check package sandbox input" src error)

let dedup_paths = fun paths ->
  let seen = Collections.HashSet.create () in
  List.filter
    paths
    ~fn:(fun path ->
      Collections.HashSet.insert seen ~value:(Path.to_string path))

let package_input_paths = fun (package: Riot_model.Package.t) ->
  Path.v "riot.toml"
  :: package.sources.src
  @ package.sources.native
  @ package.sources.tests
  @ package.sources.examples
  @ package.sources.bench
  @ List.map package.binaries ~fn:(fun binary -> binary.Riot_model.Package.path)
  |> dedup_paths

let write_hash_path = fun hasher path -> Crypto.Sha256.write hasher (Path.to_string path)

let execution_hash = fun (input: Package_planning.input) ->
  let hasher = Crypto.Sha256.create () in
  Crypto.Sha256.write hasher "riot-build2-package-sandbox-execution:v1";
  Crypto.Sha256.write hasher (Riot_model.Package_name.to_string input.package.name);
  Crypto.Sha256.write hasher input.profile.name;
  Crypto.Sha256.write hasher (Riot_model.Target.to_string input.target);
  Crypto.Sha256.write_hash hasher input.package_hash;
  write_hash_path hasher input.package.path;
  Crypto.Sha256.write hasher (UUID.to_string (UUID.v4 ()));
  Crypto.Sha256.finish hasher

let sandbox_root = fun t (input: Package_planning.input) ->
  let base =
    Riot_model.Riot_dirs.sandbox_dir_in_workspace
      ~workspace:t.workspace
      ~profile:input.profile.name
      ~target:input.target
  in
  let package = Riot_model.Package_name.to_string input.package.name in
  let name = package ^ "-" ^ Crypto.Digest.hex (execution_hash input) in
  Path.(base / Path.v name)
  |> absolute_path

let direct_and_transitive_deps = fun depset ->
  let seen = Collections.HashSet.create () in
  depset
  @ Riot_planner.Dependency.transitive_closure depset
  |> List.filter
    ~fn:(fun dep ->
      let name =
        Riot_model.Package_name.to_string dep.Riot_planner.Dependency.package.name
      in
      Collections.HashSet.insert seen ~value:name)

let materialize_dependency = fun t ~current_package ~root dep ->
  let package = dep.Riot_planner.Dependency.package in
  let target_dir = Path.(root / deps_dir package.name) in
  Riot_store.Store.promote t.store dep.Riot_planner.Dependency.input_hash ~target_dir
  |> Result.map_err
    ~fn:(fun error ->
      store_error
        ~package:current_package
        (Riot_store.Store.error_message error))

let prepare_root = fun t (input: Package_planning.input) ~depset ->
  let package = input.package in
  let* root = sandbox_root t input in
  let* () = ensure_dir root in
  let* () = ensure_dir Path.(root / check_dir) in
  let* () = ensure_dir Path.(root / link_dir) in
  let* () = ensure_dir Path.(root / Path.v ".deps") in
  let* () =
    package_input_paths package
    |> List.fold_left
      ~init:(Ok ())
      ~fn:(fun acc path ->
        let* () = acc in
        copy_package_file package ~root path)
  in
  let* () =
    direct_and_transitive_deps depset
    |> List.fold_left
      ~init:(Ok ())
      ~fn:(fun acc dep ->
        let* () = acc in
        materialize_dependency t ~current_package:package.name ~root dep)
  in
  Ok { build = input.build; package; root }

let prepare = fun t (input: Package_planning.input) ~depset ->
  match ConcurrentHashMap.get t.sandboxes ~key:input.Package_planning.build with
  | Some prepared -> Ok prepared.root
  | None ->
      let* prepared = prepare_root t input ~depset in
      ignore (
        ConcurrentHashMap.insert
          t.sandboxes
          ~key:prepared.build
          ~value:prepared
      );
      Ok prepared.root

let cleanup_success = fun t build ->
  match ConcurrentHashMap.remove t.sandboxes ~key:build with
  | None -> ()
  | Some prepared ->
      ignore prepared.package;
      ignore (Fs.remove_dir_all prepared.root)
