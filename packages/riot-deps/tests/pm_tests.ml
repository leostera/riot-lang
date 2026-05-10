open Std
open Riot_model

module Test = Std.Test
module Package_manifest = Package_manifest

let package_name = fun name ->
  Package_name.from_string name
  |> Result.expect ~msg:("Expected valid package name: " ^ name)

let workspace_manager = fun () -> Workspace_manager.create ()

let dependency = fun name source: Package.dependency -> { name = package_name name; source }

let has_name = fun expected actual -> Package_name.equal actual (package_name expected)

let source = fun ?(workspace = false) ?(builtin = false) ?path ?source_locator ?ref_ ?version () ->
  Package.{
    workspace;
    builtin;
    path;
    source_locator;
    ref_;
    version;
  }

let make_sources = fun () ->
  Package.{
    src = [];
    native = [];
    tests = [];
    examples = [];
    bench = [];
  }

let make_package = fun
  ?(dependencies = []) ?(build_dependencies = []) ?(dev_dependencies = []) ~name ~path () ->
  let name = package_name name in
  let publish =
    Package.{
      version = Some (Std.Version.make ~major:0 ~minor:1 ~patch:0 ());
      description = Some ("Package " ^ Package_name.to_string name);
      license = Some "Apache-2.0";
      is_public = Some true;
    }
  in
  Package.make
    ~name
    ~path
    ~relative_path:path
    ~dependencies
    ~dev_dependencies
    ~build_dependencies
    ~sources:(make_sources ())
    ~publish
    ()

let make_registry_cache = fun () ->
  Pkgs_ml.Registry_cache.create
    ~riot_home:(Path.v "/Users/example/.riot")
    ~registry_name:"pkgs.ml"
    ()
  |> Result.expect ~msg:"expected registry cache to initialize"

let make_registry_cache_at = fun riot_home ->
  Pkgs_ml.Registry_cache.create ~riot_home ~registry_name:"pkgs.ml" ()
  |> Result.expect ~msg:"expected registry cache to initialize"

let make_release = fun ?(dependencies = []) ?(yanked = false) ~version () ->
  Pkgs_ml.Sparse_index.{
    version;
    published_at = "2026-04-01T00:00:00Z";
    canonical_locator = "github.com/example/" ^ version;
    repo_url = "https://github.com/example/repo";
    subdir = ".";
    artifact_sha256 = "deadbeef";
    description = None;
    license = Some "Apache-2.0";
    homepage = None;
    repository = Some "https://github.com/example/repo";
    root_module = None;
    categories = [];
    keywords = [];
    manifest_key = "manifests/" ^ version ^ ".json";
    source_key = "sources/" ^ version ^ ".tar.gz";
    dependencies;
    yanked;
    yanked_at =
      if yanked then
        Some "2026-04-06T10:00:00.000Z"
      else
        None;
    yanked_by_github_login =
      if yanked then
        Some "leostera"
      else
        None;
  }

let make_registry_dependency = fun name ->
  Pkgs_ml.Sparse_index.{ name; raw = Data.Json.Object [ ("name", Data.Json.String name); ] }

let make_registry_document = fun ?(releases = []) ~name ~latest () ->
  Pkgs_ml.Sparse_index.{
    schema_version = 1;
    name;
    latest;
    updated_at = "2026-04-01T00:00:00Z";
    releases;
  }

let make_registry = fun packages ->
  Pkgs_ml.Registry.in_memory
    ~cache:(make_registry_cache ())
    ~packages
    ()

let make_registry_with_releases = fun ~packages ~releases ->
  Pkgs_ml.Registry.in_memory
    ~cache:(make_registry_cache ())
    ~packages
    ~releases
    ()

let make_release_source = fun
  ?(files = []) ~package_name ~version manifest_toml: Pkgs_ml.Registry.release_source ->
  {
    package_name;
    version;
    manifest_toml;
    files;
  }

let write_package_manifest = fun ~root contents ->
  Fs.create_dir_all root
  |> Result.expect ~msg:"expected package root to be created";
  Fs.write contents Path.(root / Path.v "riot.toml")
  |> Result.expect ~msg:"expected package manifest to be written"

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let workspace_package = fun ~workspace_root (pkg: Package.t) ->
  match Path.strip_prefix pkg.path ~prefix:workspace_root with
  | Ok relative_path ->
      Package.make
        ~name:pkg.name
        ~path:pkg.path
        ~relative_path
        ~dependencies:pkg.dependencies
        ~dev_dependencies:pkg.dev_dependencies
        ~build_dependencies:pkg.build_dependencies
        ~foreign_dependencies:pkg.foreign_dependencies
        ~binaries:pkg.binaries
        ?library:pkg.library
        ~sources:pkg.sources
        ~compiler:pkg.compiler
        ~commands:pkg.commands
        ~fix_providers:pkg.fix_providers
        ~publish:pkg.publish
        ()
  | Error _ -> pkg

let manifests_of_packages = fun packages -> List.map packages ~fn:Package_manifest.from_package

let make_workspace_manifest = fun
  ?(workspace_root = Path.v "/workspace")
  ?(dependencies = [])
  ?(dev_dependencies = [])
  ?(build_dependencies = [])
  packages ->
  let packages = List.map packages ~fn:(workspace_package ~workspace_root) in
  Riot_model.Workspace_manifest.make_realized
    ~root:workspace_root
    ~packages
    ~dependencies
    ~dev_dependencies
    ~build_dependencies
    ()

let make_workspace = fun
  ?(workspace_root = Path.v "/workspace")
  ?(dependencies = [])
  ?(dev_dependencies = [])
  ?(build_dependencies = [])
  packages ->
  let packages = List.map packages ~fn:(workspace_package ~workspace_root) in
  Riot_model.Workspace.make_realized
    ~root:workspace_root
    ~packages
    ~dependencies
    ~dev_dependencies
    ~build_dependencies
    ()

let run_lock_deps = fun
  ?emit
  ?(registry = make_registry [])
  ?(workspace_root = Path.v "/workspace")
  ~mode
  ~existing_lock
  packages ->
  let workspace = make_workspace_manifest ~workspace_root packages in
  Riot_deps.Dep_solver.lock_deps ?emit ~mode ~registry ~existing_lock ~workspace ()

let ensure_lock = fun
  ?emit ?(registry = make_registry []) ?(workspace_root = Path.v "/workspace") packages ->
  let workspace = make_workspace_manifest ~workspace_root packages in
  Riot_deps.ensure_lock
    ?emit
    ~workspace_manager:(workspace_manager ())
    ~mode:Riot_deps.Dep_solver.Refresh
    ~registry
    ~workspace
    ()

let collect_event_names = fun fn ->
  let names = ref [] in
  let emit event =
    names := Riot_model.Event.name (Riot_model.Event.Deps event) :: !names
  in
  match fn emit with
  | Ok value -> Ok (value, List.reverse !names)
  | Error err -> Error err

let pm_error_message = Riot_model.Pm_error.message

let write_file = fun path contents ->
  let parent =
    match Path.parent path with
    | Some parent -> parent
    | None -> Path.v "."
  in
  Fs.create_dir_all parent
  |> Result.expect ~msg:"expected parent directory to be created";
  Fs.write contents path
  |> Result.expect ~msg:"expected file to be written"

let list_tar_entries = fun artifact_path ->
  match Command.make "tar" ~args:[ "-tzf"; Path.to_string artifact_path ]
  |> Command.output with
  | Error (Command.SystemError err) -> Error ("failed to spawn tar: " ^ err)
  | Ok output when not (Int.equal output.status 0) ->
      Error ("failed to list artifact entries: " ^ output.stderr)
  | Ok output ->
      Ok (
        String.split output.stdout ~by:"\n"
        |> List.filter ~fn:(fun line -> not (String.equal line ""))
      )

let run_git = fun ~cwd args ->
  let command =
    Command.make
      "env"
      ~args:([
        "-u";
        "GIT_DIR";
        "-u";
        "GIT_WORK_TREE";
        "-u";
        "GIT_INDEX_FILE";
        "git";
        "-C";
        Path.to_string cwd;
      ]
      @ args)
  in
  match Command.output command with
  | Error (Command.SystemError err) -> Error ("failed to spawn git: " ^ err)
  | Ok output when not (Int.equal output.status 0) ->
      let detail =
        if String.equal output.stderr "" then
          output.stdout
        else
          output.stderr
      in
      Error ("git command failed: " ^ detail)
  | Ok output -> Ok (String.trim output.stdout)

let run_git_steps = fun ~cwd commands ->
  let rec loop outputs = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse outputs)
    | args :: rest ->
        match run_git ~cwd args with
        | Ok output -> loop (output :: outputs) rest
        | Error _ as err -> err
  in
  loop [] commands

let prepare_local_git_repo = fun ~root ?subdir ~package_name ?(version = "0.0.1") () ->
  let repo_root = root in
  let package_root =
    match subdir with
    | Some subdir -> Path.(repo_root / subdir)
    | None -> repo_root
  in
  let manifest_path = Path.(package_root / Path.v "riot.toml") in
  let source_path = Path.(package_root / Path.v "src" / Path.v (package_name ^ ".ml")) in
  let _ =
    match Fs.exists repo_root with
    | Ok true -> Fs.remove_dir_all repo_root
    | _ -> Ok ()
  in
  Fs.create_dir_all package_root
  |> Result.expect ~msg:"expected git dependency package root to be created";
  write_file
    manifest_path
    ("[package]\n"
    ^ "name = \""
    ^ package_name
    ^ "\"\n"
    ^ "version = \""
    ^ version
    ^ "\"\n"
    ^ "description = \""
    ^ package_name
    ^ "\"\n"
    ^ "license = \"Apache-2.0\"\n"
    ^ "public = true\n");
  write_file source_path "let answer = 42\n";
  run_git_steps
    ~cwd:repo_root
    [
      [ "init"; "-b"; "main" ];
      [ "config"; "user.email"; "riot-tests@example.com" ];
      [ "config"; "user.name"; "Riot Tests" ];
      [ "config"; "commit.gpgsign"; "false" ];
      [ "add"; "." ];
      [ "commit"; "-m"; "init" ];
    ]
  |> Result.map ~fn:(fun _ -> repo_root)

type recorded_request = {
  method_: string;
  url: string;
  headers: (string * string) list;
  body: string option;
}

let make_fetch_recorder = fun
  ?(post_handler = fun _uri ~headers:_ ~body:_ -> Error "unexpected POST") get_handler ->
  let requests = ref [] in
  let record ~method_ uri ~headers ~body =
    requests := {
      method_;
      url = Net.Uri.to_string uri;
      headers;
      body;
    } :: !requests
  in
  let fetch =
    Pkgs_ml.Registry.make_fetch
      ~get:(fun uri ->
        record ~method_:"GET" uri ~headers:[] ~body:None;
        get_handler uri)
      ~post:(fun uri ~headers ~body ->
        record ~method_:"POST" uri ~headers ~body:(Some body);
        post_handler uri ~headers ~body)
      ()
  in
  (fetch, requests)

let test_publisher_rejects_path_only_runtime_dependencies = fun _ctx ->
  let package =
    make_package
      ~name:"demo"
      ~path:(Path.v "/workspace/packages/demo")
      ~dependencies:[ dependency "std" (source ~path:(Path.v "../std") ()) ]
      ()
  in
  match Riot_deps.Publisher.validate_runtime_dependencies ~package with
  | Ok () -> Error "expected path-only runtime dependency to be rejected for publish"
  | Error (
    Riot_deps.Publisher.RuntimeDependencyNotPublishable { dependency; reason = `PathOnly path; _ }
  ) ->
      if String.equal dependency "std" && Path.equal path (Path.v "../std") then
        Ok ()
      else
        Error "unexpected path-only runtime dependency payload"
  | Error err -> Error ("unexpected publish validation error: " ^ Riot_deps.Publisher.message err)

let test_publisher_allows_path_with_version_runtime_dependencies = fun _ctx ->
  let package =
    make_package
      ~name:"demo"
      ~path:(Path.v "/workspace/packages/demo")
      ~dependencies:[
        dependency "std" (source ~path:(Path.v "../std") ~version:Std.Version.any ());
      ]
      ()
  in
  match Riot_deps.Publisher.validate_runtime_dependencies ~package with
  | Ok () -> Ok ()
  | Error err ->
      Error ("expected path+version runtime dependency to be publishable: "
      ^ Riot_deps.Publisher.message err)

let test_publisher_creates_package_root_tarball = fun _ctx ->
  with_tempdir
    "riot_deps_publish_tarball"
    (fun root ->
      let package_root = Path.(root / Path.v "packages/demo") in
      write_file
        Path.(package_root / Path.v "riot.toml")
        {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
      write_file Path.(package_root / Path.v "src/demo.ml") "let answer = 42\n";
      write_file Path.(package_root / Path.v "README.md") "# Demo\n";
      write_file Path.(package_root / Path.v "_build/ignore.txt") "ignore\n";
      write_file Path.(package_root / Path.v ".git/config") "ignore\n";
      write_file Path.(package_root / Path.v "node_modules/left-pad.js") "ignore\n";
      write_file Path.(package_root / Path.v ".DS_Store") "ignore\n";
      write_file Path.(package_root / Path.v "src/._demo.ml") "ignore\n";
      write_file Path.(package_root / Path.v "__MACOSX/._demo.ml") "ignore\n";
      let package = make_package ~name:"demo" ~path:package_root () in
      match Riot_deps.Publisher.create_artifact
        ~target_dir_root:root
        ~package
        ~version:(Std.Version.make ~major:0 ~minor:1 ~patch:0 ()) with
      | Error err ->
          Error ("expected artifact creation to succeed: " ^ Riot_deps.Publisher.message err)
      | Ok artifact ->
          match list_tar_entries artifact with
          | Error _ as err -> err
          | Ok entries ->
              let entries = List.sort entries ~compare:String.compare in
              let expected =
                List.sort [ "README.md"; "src/demo.ml"; "riot.toml" ] ~compare:String.compare
              in
              if entries = expected then
                Ok ()
              else
                Error ("unexpected publish artifact entries: " ^ String.concat "," entries))

let test_publisher_rejects_symlink_entries = fun _ctx ->
  with_tempdir
    "riot_deps_publish_symlink"
    (fun root ->
      let package_root = Path.(root / Path.v "packages/demo") in
      write_file
        Path.(package_root / Path.v "riot.toml")
        {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
      write_file Path.(package_root / Path.v "src/demo.ml") "let answer = 42\n";
      let link = Path.(package_root / Path.v "README.md") in
      Fs.symlink ~src:(Path.v "src/demo.ml") ~dst:link
      |> Result.expect ~msg:"expected symlink to be created";
      let package = make_package ~name:"demo" ~path:package_root () in
      match Riot_deps.Publisher.create_artifact
        ~target_dir_root:root
        ~package
        ~version:(Std.Version.make ~major:0 ~minor:1 ~patch:0 ()) with
      | Ok _ -> Error "expected publisher to reject symlink entries"
      | Error (Riot_deps.Publisher.SymlinkNotAllowed { path }) ->
          if Path.equal path link then
            Ok ()
          else
            Error "unexpected symlink rejection path"
      | Error err -> Error ("unexpected publish artifact error: " ^ Riot_deps.Publisher.message err))

let test_publisher_publishes_prepared_artifact = fun _ctx ->
  with_tempdir
    "riot_deps_publish_prepared"
    (fun root ->
      let package_root = Path.(root / Path.v "packages/demo") in
      write_file
        Path.(package_root / Path.v "riot.toml")
        {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
      write_file Path.(package_root / Path.v "src/demo.ml") "let answer = 42\n";
      let package = make_package ~name:"demo" ~path:package_root () in
      let (fetch, requests) =
        make_fetch_recorder
          ~post_handler:(fun _uri ~headers:_ ~body:_ ->
            Ok {
              Pkgs_ml.Registry.status_code = 200;
              body = {|{
  "artifact_sha256": "deadbeef",
  "package_name": "demo",
  "package_version": "0.1.0",
  "manifest": {
    "key": "packages/demo/0.1.0/deadbeef.manifest.json",
    "url": "https://cdn.pkgs.ml/packages/demo/0.1.0/deadbeef.manifest.json"
  },
  "source_archive": {
    "key": "sources/demo/0.1.0/deadbeef.tar.gz",
    "url": "https://cdn.pkgs.ml/sources/demo/0.1.0/deadbeef.tar.gz"
  },
  "claim": {
    "key": "claims/demo.json",
    "created": true
  },
  "release": {
    "key": "releases/demo/0.1.0.json",
    "created": true
  },
  "materialization": {
    "manifest": false,
    "source": false
  }
}|};
            })
          (fun uri -> Error ("unexpected GET " ^ Net.Uri.to_string uri))
      in
      let registry = Pkgs_ml.Registry.filesystem ~fetch (make_registry_cache ()) in
      let plan: Riot_deps.Publisher.publish_plan = {
        package;
        version = Std.Version.make ~major:0 ~minor:1 ~patch:0 ();
        locator = "github.com/example/demo";
        selector = "main";
      }
      in
      match Riot_deps.Publisher.prepare_publish_artifact ~target_dir_root:root plan with
      | Error err ->
          Error ("expected publish artifact preparation to succeed: "
          ^ Riot_deps.Publisher.message err)
      | Ok prepared ->
          match Riot_deps.Publisher.publish_prepared ~registry ~api_token:"root-secret" prepared with
          | Error err -> Error ("expected publish to succeed: " ^ Riot_deps.Publisher.message err)
          | Ok published ->
              match List.reverse !requests with
              | [ request ] ->
                  let has_header name value =
                    List.any
                      request.headers
                      ~fn:(fun (header_name, header_value) ->
                        String.equal header_name name && String.equal header_value value)
                  in
                  if
                    String.equal request.method_ "POST"
                    && String.equal request.url "https://api.pkgs.ml/v1/publish"
                    && has_header "authorization" "Bearer root-secret"
                    && has_header "content-type" "application/gzip"
                    && String.equal published.package_name "demo"
                    && String.equal published.package_version "0.1.0"
                  then
                    Ok ()
                  else
                    Error "unexpected publish request or response"
              | _ -> Error "expected exactly one publish request")

let test_publisher_bubbles_registry_publish_errors = fun _ctx ->
  with_tempdir
    "riot_deps_publish_registry_error"
    (fun root ->
      let package_root = Path.(root / Path.v "packages/demo") in
      write_file
        Path.(package_root / Path.v "riot.toml")
        {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
      write_file Path.(package_root / Path.v "src/demo.ml") "let answer = 42\n";
      let package = make_package ~name:"demo" ~path:package_root () in
      let (fetch, _requests) =
        make_fetch_recorder
          ~post_handler:(fun _uri ~headers:_ ~body:_ ->
            Ok {
              Pkgs_ml.Registry.status_code = 404;
              body = {|{
  "error": "package_not_found",
  "message": "package `demo` was not found in registry `pkgs.ml`"
}|};
            })
          (fun uri -> Error ("unexpected GET " ^ Net.Uri.to_string uri))
      in
      let registry = Pkgs_ml.Registry.filesystem ~fetch (make_registry_cache ()) in
      let plan: Riot_deps.Publisher.publish_plan = {
        package;
        version = Std.Version.make ~major:0 ~minor:1 ~patch:0 ();
        locator = "github.com/example/demo";
        selector = "main";
      }
      in
      match Riot_deps.Publisher.prepare_publish_artifact ~target_dir_root:root plan with
      | Error err ->
          Error ("expected publish artifact preparation to succeed: "
          ^ Riot_deps.Publisher.message err)
      | Ok prepared ->
          match Riot_deps.Publisher.publish_prepared ~registry ~api_token:"root-secret" prepared with
          | Ok _ -> Error "expected publish to bubble registry error"
          | Error (Riot_deps.Publisher.RegistryPublishFailed { locator; error }) ->
              if
                String.equal locator "github.com/example/demo"
                && String.equal error "package `demo` was not found in registry `pkgs.ml`"
              then
                Ok ()
              else
                Error "unexpected registry publish error payload"
          | Error err -> Error ("unexpected publish error: " ^ Riot_deps.Publisher.message err))

let test_publisher_reports_missing_prepared_artifact = fun _ctx ->
  with_tempdir
    "riot_deps_publish_missing_artifact"
    (fun root ->
      let artifact_path = Path.(root / Path.v "missing.tar.gz") in
      let package = make_package ~name:"demo" ~path:root () in
      let (fetch, _requests) =
        make_fetch_recorder
          ~post_handler:(fun _uri ~headers:_ ~body:_ ->
            Error "publish should not be called when the artifact is missing")
          (fun uri -> Error ("unexpected GET " ^ Net.Uri.to_string uri))
      in
      let registry = Pkgs_ml.Registry.filesystem ~fetch (make_registry_cache ()) in
      let prepared: Riot_deps.Publisher.prepared_publish = {
        package;
        version = Std.Version.make ~major:0 ~minor:1 ~patch:0 ();
        locator = "github.com/example/demo";
        selector = "main";
        artifact_path;
      }
      in
      match Riot_deps.Publisher.publish_prepared ~registry ~api_token:"root-secret" prepared with
      | Error (Riot_deps.Publisher.ArtifactReadFailed { path; _ }) ->
          if Path.equal path artifact_path then
            Ok ()
          else
            Error "expected missing artifact error to preserve artifact path"
      | Error err -> Error ("unexpected publish error: " ^ Riot_deps.Publisher.message err)
      | Ok _ -> Error "expected missing prepared artifact to fail before publish")

let test_publisher_workspace_publish_order_uses_runtime_local_dependencies = fun _ctx ->
  let core = make_package ~name:"core" ~path:(Path.v "packages/core") () in
  let util =
    make_package
      ~name:"util"
      ~path:(Path.v "packages/util")
      ~dependencies:[ dependency "core" (source ~workspace:true ()) ]
      ()
  in
  let app =
    make_package
      ~name:"app"
      ~path:(Path.v "packages/app")
      ~dependencies:[
        dependency "util" (source ~path:(Path.v "../util") ~version:Std.Version.any ());
      ]
      ()
  in
  match Riot_deps.Publisher.workspace_publish_order ~packages:[ app; util; core ] with
  | Error err -> Error ("expected publish order to succeed: " ^ Riot_deps.Publisher.message err)
  | Ok ordered ->
      if
        List.map ordered ~fn:(fun (pkg: Package.t) -> pkg.name)
        = [ package_name "core"; package_name "util"; package_name "app" ]
      then
        Ok ()
      else
        Error "unexpected workspace publish order"

let test_publisher_workspace_publish_order_ignores_dev_and_build_dependencies = fun _ctx ->
  let core = make_package ~name:"core" ~path:(Path.v "packages/core") () in
  let app =
    make_package
      ~name:"app"
      ~path:(Path.v "packages/app")
      ~build_dependencies:[ dependency "core" (source ~workspace:true ()) ]
      ~dev_dependencies:[ dependency "core" (source ~workspace:true ()) ]
      ()
  in
  match Riot_deps.Publisher.workspace_publish_order ~packages:[ app; core ] with
  | Error err -> Error ("expected publish order to succeed: " ^ Riot_deps.Publisher.message err)
  | Ok ordered ->
      if
        List.map ordered ~fn:(fun (pkg: Package.t) -> pkg.name)
        = [ package_name "app"; package_name "core" ]
      then
        Ok ()
      else
        Error "expected workspace publish order to ignore dev/build edges"

let test_publisher_workspace_publish_order_reports_cycles = fun _ctx ->
  let a =
    make_package
      ~name:"a"
      ~path:(Path.v "packages/a")
      ~dependencies:[ dependency "b" (source ~workspace:true ()) ]
      ()
  in
  let b =
    make_package
      ~name:"b"
      ~path:(Path.v "packages/b")
      ~dependencies:[ dependency "a" (source ~workspace:true ()) ]
      ()
  in
  match Riot_deps.Publisher.workspace_publish_order ~packages:[ a; b ] with
  | Ok _ -> Error "expected cyclic workspace publish order to fail"
  | Error (Riot_deps.Publisher.CyclicWorkspacePublishOrder _) -> Ok ()
  | Error err -> Error ("unexpected publish order error: " ^ Riot_deps.Publisher.message err)

let test_publisher_validate_registry_dependencies_skips_workspace_publish_set = fun _ctx ->
  let core = make_package ~name:"core" ~path:(Path.v "packages/core") () in
  let app =
    make_package
      ~name:"app"
      ~path:(Path.v "packages/app")
      ~dependencies:[
        dependency "core" (source ~path:(Path.v "../core") ~version:Std.Version.any ());
      ]
      ()
  in
  let registry = make_registry [] in
  match Riot_deps.Publisher.validate_registry_dependencies
    ~registry
    ~publishing_workspace_packages:[ core.name; app.name ]
    ~package:app with
  | Ok () -> Ok ()
  | Error err ->
      Error ("expected workspace publish set to skip registry lookup: "
      ^ Riot_deps.Publisher.message err)

let test_git_provenance_discovers_nested_package_locator = fun _ctx ->
  with_tempdir
    "riot_deps_git_provenance_nested"
    (fun root ->
      let package_root = Path.(root / Path.v "packages/demo") in
      write_file
        Path.(package_root / Path.v "riot.toml")
        {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
      write_file Path.(package_root / Path.v "src/demo.ml") "let answer = 42\n";
      match run_git_steps
        ~cwd:root
        [
          [ "init"; "-q" ];
          [ "config"; "user.email"; "demo@example.com" ];
          [ "config"; "user.name"; "Demo" ];
          [ "remote"; "add"; "origin"; "https://github.com/example/riot.git"; ];
          [ "add"; "." ];
          [ "-c"; "commit.gpgsign=false"; "commit"; "-qm"; "init"; ];
        ] with
      | Ok _ ->
          let canonical_root =
            Fs.canonicalize root
            |> Result.expect ~msg:"expected temp repo root to canonicalize"
          in
          (
            match Riot_deps.Git_provenance.discover ~package_root with
            | Error err ->
                Error ("expected git provenance discovery to succeed: "
                ^ Riot_deps.Git_provenance.message err)
            | Ok provenance ->
                if
                  String.equal provenance.locator "github.com/example/riot/packages/demo"
                  && Path.equal provenance.repository_root canonical_root
                  && provenance.package_subdir = Some (Path.v "packages/demo")
                  && String.equal provenance.origin_url "https://github.com/example/riot.git"
                  && String.length provenance.selector = 40
                then
                  Ok ()
                else
                  Error "unexpected nested git provenance"
          )
      | Error err -> Error err)

let test_git_provenance_discovers_repo_root_locator = fun _ctx ->
  with_tempdir
    "riot_deps_git_provenance_root"
    (fun root ->
      write_file
        Path.(root / Path.v "riot.toml")
        {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
      write_file Path.(root / Path.v "src/demo.ml") "let answer = 42\n";
      match run_git_steps
        ~cwd:root
        [
          [ "init"; "-q" ];
          [ "config"; "user.email"; "demo@example.com" ];
          [ "config"; "user.name"; "Demo" ];
          [ "remote"; "add"; "origin"; "git@github.com:example/demo.git"; ];
          [ "add"; "." ];
          [ "-c"; "commit.gpgsign=false"; "commit"; "-qm"; "init"; ];
        ] with
      | Ok _ ->
          let canonical_root =
            Fs.canonicalize root
            |> Result.expect ~msg:"expected temp repo root to canonicalize"
          in
          (
            match Riot_deps.Git_provenance.discover ~package_root:root with
            | Error err ->
                Error ("expected git provenance discovery to succeed: "
                ^ Riot_deps.Git_provenance.message err)
            | Ok provenance ->
                if
                  String.equal provenance.locator "github.com/example/demo"
                  && Path.equal provenance.repository_root canonical_root
                  && provenance.package_subdir = None
                  && String.equal provenance.origin_url "git@github.com:example/demo.git"
                  && String.length provenance.selector = 40
                then
                  Ok ()
                else
                  Error "unexpected root git provenance"
          )
      | Error err -> Error err)

let test_git_provenance_reports_non_git_repository = fun _ctx ->
  with_tempdir
    "riot_deps_git_provenance_not_git"
    (fun root ->
      match Riot_deps.Git_provenance.discover ~package_root:root with
      | Error (Riot_deps.Git_provenance.NotGitRepository { path }) ->
          let canonical_root =
            Fs.canonicalize root
            |> Result.expect ~msg:"expected temp root to canonicalize"
          in
          if Path.equal path canonical_root then
            Ok ()
          else
            Error "expected not-git-repository error to preserve package root"
      | Error err ->
          Error ("expected not-git-repository error, got: " ^ Riot_deps.Git_provenance.message err)
      | Ok _ -> Error "expected git provenance discovery outside a git repository to fail")

let test_publisher_publish_discovers_git_provenance = fun _ctx ->
  with_tempdir
    "riot_deps_publish_with_git_provenance"
    (fun root ->
      let package_root = Path.(root / Path.v "packages/demo") in
      write_file
        Path.(package_root / Path.v "riot.toml")
        {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
      write_file Path.(package_root / Path.v "src/demo.ml") "let answer = 42\n";
      match run_git_steps
        ~cwd:root
        [
          [ "init"; "-q" ];
          [ "config"; "user.email"; "demo@example.com" ];
          [ "config"; "user.name"; "Demo" ];
          [ "remote"; "add"; "origin"; "https://github.com/example/riot.git"; ];
          [ "add"; "." ];
          [ "-c"; "commit.gpgsign=false"; "commit"; "-qm"; "init"; ];
        ] with
      | Ok _ -> (
          match run_git ~cwd:package_root [ "rev-parse"; "HEAD" ] with
          | Error err -> Error err
          | Ok selector ->
              let package = make_package ~name:"demo" ~path:package_root () in
              let (fetch, requests) =
                make_fetch_recorder
                  ~post_handler:(fun _uri ~headers:_ ~body:_ ->
                    Ok {
                      Pkgs_ml.Registry.status_code = 200;
                      body = {|{
  "artifact_sha256": "deadbeef",
  "package_name": "demo",
  "package_version": "0.1.0",
  "manifest": {
    "key": "packages/demo/0.1.0/deadbeef.manifest.json",
    "url": "https://cdn.pkgs.ml/packages/demo/0.1.0/deadbeef.manifest.json"
  },
  "source_archive": {
    "key": "sources/demo/0.1.0/deadbeef.tar.gz",
    "url": "https://cdn.pkgs.ml/sources/demo/0.1.0/deadbeef.tar.gz"
  },
  "claim": {
    "key": "claims/demo.json",
    "created": true
  },
  "release": {
    "key": "releases/demo/0.1.0.json",
    "created": true
  },
  "materialization": {
    "manifest": false,
    "source": false
  }
}|};
                    })
                  (fun uri -> Error ("unexpected GET " ^ Net.Uri.to_string uri))
              in
              let registry = Pkgs_ml.Registry.filesystem ~fetch (make_registry_cache ()) in
              (
                match Riot_deps.Publisher.publish
                  ~registry
                  ~target_dir_root:root
                  ~publishing_workspace_packages:[]
                  ~package
                  ~api_token:"root-secret" with
                | Error err ->
                    Error ("expected publish to succeed: " ^ Riot_deps.Publisher.message err)
                | Ok published ->
                    if
                      String.equal published.package_name "demo"
                      && List.any
                        !requests
                        ~fn:(fun request ->
                          String.equal request.method_ "POST" && String.length request.url > 0)
                    then
                      Ok ()
                    else
                      Error "unexpected publish request discovered from git provenance"
              )
        )
      | Error err -> Error err)

let test_publisher_prepare_publish_discovers_git_provenance_without_registry = fun _ctx ->
  with_tempdir
    "riot_deps_prepare_publish"
    (fun root ->
      let package_root = Path.(root / Path.v "packages/demo") in
      write_file
        Path.(package_root / Path.v "riot.toml")
        {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
      write_file Path.(package_root / Path.v "src/demo.ml") "let answer = 42\n";
      match run_git_steps
        ~cwd:root
        [
          [ "init"; "-q" ];
          [ "config"; "user.email"; "demo@example.com" ];
          [ "config"; "user.name"; "Demo" ];
          [ "remote"; "add"; "origin"; "https://github.com/example/riot.git"; ];
          [ "add"; "." ];
          [ "-c"; "commit.gpgsign=false"; "commit"; "-qm"; "init"; ];
        ] with
      | Ok _ -> (
          match run_git ~cwd:package_root [ "rev-parse"; "HEAD" ] with
          | Error err -> Error err
          | Ok selector ->
              let package = make_package ~name:"demo" ~path:package_root () in
              let registry = Pkgs_ml.Registry.filesystem (make_registry_cache ()) in
              (
                match Riot_deps.Publisher.prepare_publish
                  ~registry
                  ~target_dir_root:root
                  ~publishing_workspace_packages:[]
                  ~package with
                | Error err ->
                    Error ("expected prepare_publish to succeed: " ^ Riot_deps.Publisher.message err)
                | Ok prepared ->
                    if
                      has_name "demo" prepared.package.name
                      && String.equal prepared.locator "github.com/example/riot/packages/demo"
                      && String.equal prepared.selector selector
                      && String.length (Path.to_string prepared.artifact_path) > 0
                    then
                      Ok ()
                    else
                      Error "unexpected prepared publish payload"
              )
        )
      | Error err -> Error err)

let test_lock_deps_projects_workspace_packages = fun _ctx ->
  let std_pkg = make_package ~name:"std" ~path:(Path.v "/workspace/packages/std") () in
  let app_pkg =
    make_package
      ~name:"app"
      ~path:(Path.v "/workspace/packages/app")
      ~dependencies:[ dependency "std" (source ~workspace:true ()) ]
      ~build_dependencies:[ dependency "std" (source ~workspace:true ()) ]
      ()
  in
  match run_lock_deps ~mode:Refresh ~existing_lock:None [ app_pkg; std_pkg ] with
  | Error err -> Error ("expected workspace lock projection to succeed: " ^ pm_error_message err)
  | Ok lockfile ->
      let app_lock =
        List.head lockfile.packages
        |> Option.expect ~msg:"expected app lock package"
      in
      let std_lock =
        List.get lockfile.packages ~at:1
        |> Option.expect ~msg:"expected std lock package"
      in
      if lockfile.format_version = 1
      && has_name "app" app_lock.id.name
      && has_name "std" std_lock.id.name
      && app_lock.provenance = Riot_model.Lockfile.Workspace
      && std_lock.provenance = Riot_model.Lockfile.Workspace
      && app_lock.root = Some (Path.v "packages/app")
      && std_lock.root = Some (Path.v "packages/std")
      && List.length app_lock.dependencies = 1
      && List.length app_lock.build_dependencies = 1
      && (
        (
          List.head app_lock.dependencies
          |> Option.expect ~msg:"expected app dependency"
        ).package.name
        |> has_name "std"
      ) then
        Ok ()
      else
        Error "expected workspace packages to be projected into the lockfile"

let test_lock_deps_resolves_path_dependencies = fun _ctx ->
  with_tempdir
    "riot_deps_path_dep"
    (fun workspace_root ->
      let foo_root = Path.(workspace_root / Path.v "vendor/foo") in
      write_package_manifest ~root:foo_root {|
[package]
name = "foo"
version = "1.2.3"
|};
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "foo" (source ~path:(Path.v "../../vendor/foo") ()) ]
          ()
      in
      match run_lock_deps ~workspace_root ~mode:Refresh ~existing_lock:None [ app_pkg ] with
      | Error err -> Error ("expected path dependency locking to succeed: " ^ pm_error_message err)
      | Ok lockfile ->
          let app_lock =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Lockfile.package) -> has_name "app" pkg.id.name)
          in
          let foo_lock =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Lockfile.package) -> has_name "foo" pkg.id.name)
          in
          match (app_lock, foo_lock) with
          | (Some app_lock, Some foo_lock) ->
              if List.length lockfile.packages = 2
              && (
                (
                  List.head app_lock.dependencies
                  |> Option.expect ~msg:"expected app dependency"
                ).package.name
                |> has_name "foo"
              )
              && foo_lock.root = Some (Path.v "vendor/foo")
              && foo_lock.provenance = Riot_model.Lockfile.Path (Path.v "../../vendor/foo") then
                Ok ()
              else
                Error "expected path dependency to resolve to an exact local lock package"
          | _ -> Error "expected app and foo to appear in the lockfile")

let test_lock_deps_resolves_transitive_path_dependencies = fun _ctx ->
  with_tempdir
    "riot_deps_transitive_path_dep"
    (fun workspace_root ->
      let foo_root = Path.(workspace_root / Path.v "vendor/foo") in
      let bar_root = Path.(workspace_root / Path.v "vendor/bar") in
      write_package_manifest
        ~root:foo_root
        {|
[package]
name = "foo"
version = "1.2.3"

[dependencies]
bar = { path = "../bar" }
|};
      write_package_manifest ~root:bar_root {|
[package]
name = "bar"
version = "2.0.0"
|};
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "foo" (source ~path:(Path.v "../../vendor/foo") ()) ]
          ()
      in
      match run_lock_deps ~workspace_root ~mode:Refresh ~existing_lock:None [ app_pkg ] with
      | Error err ->
          Error ("expected transitive path dependencies to resolve: " ^ pm_error_message err)
      | Ok lockfile ->
          let foo_lock =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Lockfile.package) -> has_name "foo" pkg.id.name)
          in
          let bar_lock =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Lockfile.package) -> has_name "bar" pkg.id.name)
          in
          match (foo_lock, bar_lock) with
          | (Some foo_lock, Some bar_lock) ->
              if List.length lockfile.packages = 3
              && (
                (
                  List.head foo_lock.dependencies
                  |> Option.expect ~msg:"expected foo dependency"
                ).package.name
                |> has_name "bar"
              )
              && bar_lock.root = Some (Path.v "vendor/bar")
              && bar_lock.provenance = Riot_model.Lockfile.Path (Path.v "../bar") then
                Ok ()
              else
                Error "expected nested path dependency roots to resolve from the declaring package"
          | _ -> Error "expected both foo and bar lock packages")

let test_lock_deps_falls_back_to_registry_when_path_dependency_is_missing = fun _ctx ->
  with_tempdir
    "riot_deps_missing_path_registry_fallback"
    (fun workspace_root ->
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[
            {
              name = package_name "std";
              source = source ~path:(Path.v "../../vendor/std") ~version:Std.Version.any ();
            };
          ]
          ()
      in
      let registry =
        Pkgs_ml.Registry.in_memory
          ~cache:(make_registry_cache_at Path.(workspace_root / Path.v ".riot"))
          ~packages:[
            make_registry_document
              ~name:"std"
              ~latest:"0.2.0"
              ~releases:[ make_release ~version:"0.2.0" () ]
              ();
          ]
          ~releases:[
            make_release_source
              ~package_name:"std"
              ~version:"0.2.0"
              {|
[package]
name = "std"
version = "0.2.0"
|};
          ]
          ()
      in
      match run_lock_deps ~registry ~workspace_root ~mode:Refresh ~existing_lock:None [ app_pkg ] with
      | Error err ->
          Error ("expected missing path+version dependency to fall back to registry: "
          ^ pm_error_message err)
      | Ok lockfile ->
          let app_lock =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Lockfile.package) -> has_name "app" pkg.id.name)
          in
          let registry_std =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Riot_model.Lockfile.package) ->
                has_name "std" pkg.id.name && pkg.id.registry = Some "pkgs.ml")
          in
          let local_std =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Lockfile.package) ->
                has_name "std" pkg.id.name && pkg.id.registry = None)
          in
          match (app_lock, registry_std, local_std) with
          | (Some app_lock, Some registry_std, None) ->
              if
                List.length lockfile.packages = 2
                && (
                  List.head app_lock.dependencies
                  |> Option.expect ~msg:"expected app dependency"
                ).package
                = registry_std.id
              then
                Ok ()
              else
                Error "expected app to lock against the registry release when the local path is absent"
          | _ -> Error "expected only the registry std package to appear in the lockfile")

let test_lock_deps_collapses_workspace_path_dependencies = fun _ctx ->
  let std_pkg = make_package ~name:"std" ~path:(Path.v "/workspace/packages/std") () in
  let app_pkg =
    make_package
      ~name:"app"
      ~path:(Path.v "/workspace/packages/app")
      ~dependencies:[ dependency "std" (source ~path:(Path.v "../std") ()) ]
      ()
  in
  match run_lock_deps ~mode:Refresh ~existing_lock:None [ app_pkg; std_pkg ] with
  | Error err ->
      Error ("expected workspace path dependency to collapse to workspace package: "
      ^ pm_error_message err)
  | Ok lockfile ->
      let app_lock =
        List.find lockfile.packages ~fn:(fun (pkg: Lockfile.package) -> has_name "app" pkg.id.name)
      in
      let std_lock =
        List.find lockfile.packages ~fn:(fun (pkg: Lockfile.package) -> has_name "std" pkg.id.name)
      in
      match (app_lock, std_lock) with
      | (Some app_lock, Some std_lock) ->
          if List.length lockfile.packages = 2 && app_lock.dependencies = [
            Riot_model.Lockfile.{
              name = package_name "std";
              package =
                {
                  registry = None;
                  name = package_name "std";
                  version = None;
                  sha256 = None;
                };
            };
          ] && std_lock.provenance = Riot_model.Lockfile.Workspace then
            Ok ()
          else
            Error "expected workspace path dependency to reuse the workspace lock package"
      | _ -> Error "expected app and std workspace packages to appear in the lockfile"

let test_lock_deps_resolves_registry_dependencies_to_exact_versions = fun _ctx ->
  with_tempdir
    "riot_deps_registry_exact_versions"
    (fun workspace_root ->
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "std" (source ~version:Std.Version.any ()) ]
          ()
      in
      let registry =
        Pkgs_ml.Registry.in_memory
          ~cache:(make_registry_cache_at Path.(workspace_root / Path.v ".riot"))
          ~packages:[
            make_registry_document
              ~name:"std"
              ~latest:"0.2.0"
              ~releases:[
                make_release ~version:"0.2.0" ~dependencies:[ make_registry_dependency "kernel" ] ();
              ]
              ();
            make_registry_document
              ~name:"kernel"
              ~latest:"1.0.0"
              ~releases:[ make_release ~version:"1.0.0" () ]
              ();
          ]
          ~releases:[
            make_release_source
              ~package_name:"std"
              ~version:"0.2.0"
              {|
[package]
name = "std"
version = "0.2.0"

[dependencies]
kernel = { path = "../kernel", version = "*" }
|};
            make_release_source
              ~package_name:"kernel"
              ~version:"1.0.0"
              {|
[package]
name = "kernel"
version = "1.0.0"
|};
          ]
          ()
      in
      match run_lock_deps ~registry ~workspace_root ~mode:Refresh ~existing_lock:None [ app_pkg ] with
      | Error err ->
          Error ("expected registry dependency locking to succeed: " ^ pm_error_message err)
      | Ok lockfile ->
          let app_lock =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Lockfile.package) -> has_name "app" pkg.id.name)
          in
          let std_lock =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Riot_model.Lockfile.package) ->
                has_name "std" pkg.id.name && pkg.id.version = Some "0.2.0")
          in
          let kernel_lock =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Riot_model.Lockfile.package) ->
                has_name "kernel" pkg.id.name && pkg.id.version = Some "1.0.0")
          in
          match (app_lock, std_lock, kernel_lock) with
          | (Some app_lock, Some std_lock, Some kernel_lock) ->
              let app_dependency =
                match app_lock.dependencies with
                | [ dep ] -> Some dep.package
                | _ -> None
              in
              let std_dependency =
                match std_lock.dependencies with
                | [ dep ] -> Some dep.package
                | _ -> None
              in
              if List.length lockfile.packages = 3 && (
                match app_dependency with
                | Some dependency ->
                    has_name "std" dependency.name && dependency.version = Some "0.2.0"
                | None -> false
              ) && std_lock.id.version = Some "0.2.0" && std_lock.root = None && (
                match std_dependency with
                | Some dependency -> has_name "kernel" dependency.name
                | None -> false
              ) && kernel_lock.id.version = Some "1.0.0" then
                Ok ()
              else
                Error "expected registry dependency to resolve to exact external lock packages"
          | _ -> Error "expected workspace and transitive registry lock packages")

let test_lock_deps_reports_missing_registry_package_with_required_by = fun _ctx ->
  let app_root = Path.v "/workspace/packages/app" in
  let app_pkg =
    make_package
      ~name:"app"
      ~path:app_root
      ~dependencies:[ dependency "std" (source ~version:Std.Version.any ()) ]
      ()
  in
  match run_lock_deps ~registry:(make_registry []) ~mode:Refresh ~existing_lock:None [ app_pkg ] with
  | Ok _ -> Error "expected missing registry package to fail"
  | Error (
    Riot_deps.Error.PackageNotFound { package; registry; required_by = Some required_by }
  ) ->
      if
        String.equal package "std"
        && String.equal registry "pkgs.ml"
        && String.equal required_by.package "app"
        && required_by.path = Some app_root
      then
        Ok ()
      else
        Error "expected missing registry package error to include the requiring workspace package"
  | Error err -> Error ("expected missing registry package error, got: " ^ pm_error_message err)

let test_lock_deps_reports_missing_registry_version_with_available_versions = fun _ctx ->
  let app_root = Path.v "/workspace/packages/app" in
  let requirement =
    Std.Version.parse_requirement "0.3"
    |> Result.expect ~msg:"expected 0.3 requirement to parse"
  in
  let app_pkg =
    make_package
      ~name:"app"
      ~path:app_root
      ~dependencies:[ dependency "minttea" (source ~version:requirement ()) ]
      ()
  in
  let registry =
    make_registry
      [
        make_registry_document
          ~name:"minttea"
          ~latest:"0.2.5"
          ~releases:[ make_release ~version:"0.1.0" (); make_release ~version:"0.2.5" () ]
          ();
      ]
  in
  match run_lock_deps ~registry ~mode:Refresh ~existing_lock:None [ app_pkg ] with
  | Ok _ -> Error "expected unavailable registry version to fail"
  | Error (
    Riot_deps.Error.RegistryVersionNotFound {
      package;
      registry;
      requirement;
      available_versions;
      required_by = Some required_by;
    }
  ) ->
      if
        String.equal package "minttea"
        && String.equal registry "pkgs.ml"
        && String.equal requirement "0.3"
        && available_versions = [ "0.1.0"; "0.2.5" ]
        && String.equal required_by.package "app"
        && required_by.path = Some app_root
      then
        Ok ()
      else
        Error "expected registry version error to include requirement, available versions, and required-by"
  | Error err -> Error ("expected missing registry version error, got: " ^ pm_error_message err)

let test_lock_deps_supports_major_minor_prefix_requirements = fun _ctx ->
  let requirement =
    Std.Version.parse_requirement "0.2"
    |> Result.expect ~msg:"expected 0.2 requirement to parse"
  in
  let app_pkg =
    make_package
      ~name:"app"
      ~path:(Path.v "/workspace/packages/app")
      ~dependencies:[ dependency "minttea" (source ~version:requirement ()) ]
      ()
  in
  let registry =
    make_registry
      [
        make_registry_document
          ~name:"minttea"
          ~latest:"1.0.0"
          ~releases:[
            make_release ~version:"0.1.0" ();
            make_release ~version:"0.2.0" ();
            make_release ~version:"0.2.3" ();
            make_release ~version:"0.3.0" ();
            make_release ~version:"1.0.0" ();
          ]
          ();
      ]
  in
  match run_lock_deps ~registry ~mode:Refresh ~existing_lock:None [ app_pkg ] with
  | Error err -> Error ("expected 0.2 requirement to resolve: " ^ pm_error_message err)
  | Ok lockfile ->
      match List.find
        lockfile.packages
        ~fn:(fun (pkg: Riot_model.Lockfile.package) -> has_name "minttea" pkg.id.name) with
      | Some pkg when pkg.id.version = Some "0.2.3" -> Ok ()
      | Some pkg ->
          Error ("expected 0.2 requirement to pick highest 0.2.x release, got "
          ^ Option.unwrap_or ~default:"<none>" pkg.id.version)
      | None -> Error "expected minttea to be locked"

let test_lock_deps_supports_major_prefix_requirements = fun _ctx ->
  let requirement =
    Std.Version.parse_requirement "0"
    |> Result.expect ~msg:"expected 0 requirement to parse"
  in
  let app_pkg =
    make_package
      ~name:"app"
      ~path:(Path.v "/workspace/packages/app")
      ~dependencies:[ dependency "minttea" (source ~version:requirement ()) ]
      ()
  in
  let registry =
    make_registry
      [
        make_registry_document
          ~name:"minttea"
          ~latest:"1.0.0"
          ~releases:[
            make_release ~version:"0.1.0" ();
            make_release ~version:"0.2.0" ();
            make_release ~version:"0.9.9" ();
            make_release ~version:"1.0.0" ();
          ]
          ();
      ]
  in
  match run_lock_deps ~registry ~mode:Refresh ~existing_lock:None [ app_pkg ] with
  | Error err -> Error ("expected 0 requirement to resolve: " ^ pm_error_message err)
  | Ok lockfile ->
      match List.find
        lockfile.packages
        ~fn:(fun (pkg: Riot_model.Lockfile.package) -> has_name "minttea" pkg.id.name) with
      | Some pkg when pkg.id.version = Some "0.9.9" -> Ok ()
      | Some pkg ->
          Error ("expected 0 requirement to pick highest 0.x.y release, got "
          ^ Option.unwrap_or ~default:"<none>" pkg.id.version)
      | None -> Error "expected minttea to be locked"

let test_lock_deps_prefers_workspace_packages_over_registry_for_matching_names = fun _ctx ->
  let std_pkg = make_package ~name:"std" ~path:(Path.v "/workspace/packages/std") () in
  let app_pkg =
    make_package
      ~name:"app"
      ~path:(Path.v "/workspace/packages/app")
      ~dependencies:[ dependency "std" (source ~version:Std.Version.any ()) ]
      ()
  in
  match run_lock_deps
    ~registry:(make_registry [])
    ~mode:Refresh
    ~existing_lock:None
    [ app_pkg; std_pkg ] with
  | Error err ->
      Error ("expected workspace package to satisfy matching registry requirement locally: "
      ^ pm_error_message err)
  | Ok lockfile ->
      let app_lock =
        List.find lockfile.packages ~fn:(fun (pkg: Lockfile.package) -> has_name "app" pkg.id.name)
      in
      let std_lock =
        List.find lockfile.packages ~fn:(fun (pkg: Lockfile.package) -> has_name "std" pkg.id.name)
      in
      match (app_lock, std_lock) with
      | (Some app_lock, Some std_lock) ->
          if List.length lockfile.packages = 2 && app_lock.dependencies = [
            Riot_model.Lockfile.{
              name = package_name "std";
              package =
                {
                  registry = None;
                  name = package_name "std";
                  version = None;
                  sha256 = None;
                };
            };
          ] && std_lock.id.registry = None && std_lock.id.version = None then
            Ok ()
          else
            Error "expected matching workspace packages to win over registry resolution"
      | _ -> Error "expected app and std workspace packages to appear in the lockfile"

let test_lock_deps_prefers_available_local_packages_over_registry_dependencies = fun _ctx ->
  with_tempdir
    "riot_deps_local_beats_registry"
    (fun workspace_root ->
      let std_root = Path.(workspace_root / Path.v "vendor/std") in
      let fixme_root = Path.(workspace_root / Path.v "vendor/fixme") in
      let model_root = Path.(workspace_root / Path.v "vendor/model") in
      write_package_manifest
        ~root:std_root
        {|
[package]
name = "std"
version = "0.1.0"

[build-dependencies]
fixme = { path = "../fixme" }
|};
      write_package_manifest ~root:fixme_root {|
[package]
name = "fixme"
version = "0.1.0"
|};
      write_package_manifest
        ~root:model_root
        {|
[package]
name = "model"
version = "0.1.0"

[dependencies]
std = "*"
|};
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[
            dependency "std" (source ~path:(Path.v "../../vendor/std") ());
            dependency "model" (source ~path:(Path.v "../../vendor/model") ());
          ]
          ()
      in
      let registry =
        make_registry
          [
            make_registry_document
              ~name:"std"
              ~latest:"9.9.9"
              ~releases:[ make_release ~version:"9.9.9" () ]
              ();
          ]
      in
      match run_lock_deps ~registry ~workspace_root ~mode:Refresh ~existing_lock:None [ app_pkg ] with
      | Error err ->
          Error ("expected local path package to beat registry dependency: " ^ pm_error_message err)
      | Ok lockfile ->
          let local_std =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Riot_model.Lockfile.package) ->
                has_name "std" pkg.id.name && pkg.id.registry = None)
          in
          let registry_std =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Riot_model.Lockfile.package) ->
                has_name "std" pkg.id.name && pkg.id.registry = Some "pkgs.ml")
          in
          let model_lock =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Lockfile.package) -> has_name "model" pkg.id.name)
          in
          match (local_std, registry_std, model_lock) with
          | (Some local_std, None, Some model_lock) ->
              if
                List.length model_lock.dependencies = 1
                && (
                  List.head model_lock.dependencies
                  |> Option.expect ~msg:"expected model dependency"
                ).package
                = local_std.id
              then
                Ok ()
              else
                Error "expected version-only dependency to reuse the available local path package"
          | (Some _, Some _, _) -> Error "expected registry std to stay out of the lock graph"
          | _ -> Error "expected local std and model lock packages")

let test_lock_deps_ignores_builtin_dependencies = fun _ctx ->
  let app_pkg =
    make_package
      ~name:"app"
      ~path:(Path.v "/workspace/packages/app")
      ~dependencies:[ dependency "stdlib" (source ~builtin:true ~version:Std.Version.any ()) ]
      ()
  in
  match run_lock_deps ~registry:(make_registry []) ~mode:Refresh ~existing_lock:None [ app_pkg ] with
  | Error err -> Error ("expected builtin dependency locking to succeed: " ^ pm_error_message err)
  | Ok lockfile ->
      match lockfile.packages with
      | [ app_lock ] when has_name "app" app_lock.id.name && app_lock.dependencies = [] -> Ok ()
      | _ -> Error "expected builtin dependencies to stay out of the lock graph"

let test_lock_deps_ignores_builtin_registry_release_dependencies = fun _ctx ->
  with_tempdir
    "riot_deps_builtin_registry_deps"
    (fun workspace_root ->
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "std" (source ~version:Std.Version.any ()) ]
          ()
      in
      let registry =
        Pkgs_ml.Registry.in_memory
          ~cache:(make_registry_cache_at Path.(workspace_root / Path.v ".riot"))
          ~packages:[
            make_registry_document
              ~name:"std"
              ~latest:"0.1.0"
              ~releases:[
                make_release
                  ~version:"0.1.0"
                  ~dependencies:[
                    make_registry_dependency "stdlib";
                    make_registry_dependency "unix";
                  ]
                  ();
              ]
              ();
          ]
          ~releases:[
            make_release_source
              ~package_name:"std"
              ~version:"0.1.0"
              {|
[package]
name = "std"
version = "0.1.0"

[dependencies]
stdlib = "*"
unix = "*"
|};
          ]
          ()
      in
      match run_lock_deps ~registry ~workspace_root ~mode:Refresh ~existing_lock:None [ app_pkg ] with
      | Error err ->
          Error ("expected builtin registry dependencies to be ignored: " ^ pm_error_message err)
      | Ok lockfile ->
          let std_lock =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Riot_model.Lockfile.package) ->
                has_name "std" pkg.id.name && pkg.id.version = Some "0.1.0")
          in
          match std_lock with
          | Some pkg when pkg.dependencies = [] && List.length lockfile.packages = 2 -> Ok ()
          | Some _ ->
              Error "expected builtin registry release dependencies to stay out of the lock graph"
          | None -> Error "expected std registry package to be locked")

let test_lock_deps_handles_cyclic_registry_dependencies = fun _ctx ->
  with_tempdir
    "riot_deps_cyclic_registry"
    (fun workspace_root ->
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "foo" (source ~version:Std.Version.any ()) ]
          ()
      in
      let registry =
        Pkgs_ml.Registry.in_memory
          ~cache:(make_registry_cache_at Path.(workspace_root / Path.v ".riot"))
          ~packages:[
            make_registry_document
              ~name:"foo"
              ~latest:"1.0.0"
              ~releases:[
                make_release ~version:"1.0.0" ~dependencies:[ make_registry_dependency "bar" ] ();
              ]
              ();
            make_registry_document
              ~name:"bar"
              ~latest:"2.0.0"
              ~releases:[
                make_release ~version:"2.0.0" ~dependencies:[ make_registry_dependency "foo" ] ();
              ]
              ();
          ]
          ~releases:[
            make_release_source
              ~package_name:"foo"
              ~version:"1.0.0"
              {|
[package]
name = "foo"
version = "1.0.0"

[dependencies]
bar = { path = "../bar", version = "*" }
|};
            make_release_source
              ~package_name:"bar"
              ~version:"2.0.0"
              {|
[package]
name = "bar"
version = "2.0.0"

[dependencies]
foo = { path = "../foo", version = "*" }
|};
          ]
          ()
      in
      match run_lock_deps ~registry ~workspace_root ~mode:Refresh ~existing_lock:None [ app_pkg ] with
      | Error err ->
          Error ("expected cyclic registry dependencies to resolve: " ^ pm_error_message err)
      | Ok lockfile ->
          let foo_lock =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Riot_model.Lockfile.package) ->
                has_name "foo" pkg.id.name && pkg.id.version = Some "1.0.0")
          in
          let bar_lock =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Riot_model.Lockfile.package) ->
                has_name "bar" pkg.id.name && pkg.id.version = Some "2.0.0")
          in
          match (foo_lock, bar_lock) with
          | (Some foo_lock, Some bar_lock) ->
              let foo_dependency =
                match foo_lock.dependencies with
                | [ dep ] -> Some dep.package
                | _ -> None
              in
              let bar_dependency =
                match bar_lock.dependencies with
                | [ dep ] -> Some dep.package
                | _ -> None
              in
              if List.length lockfile.packages = 3 && (
                match foo_dependency with
                | Some dependency ->
                    has_name "bar" dependency.name && dependency.version = Some "2.0.0"
                | None -> false
              ) && (
                match bar_dependency with
                | Some dependency ->
                    has_name "foo" dependency.name && dependency.version = Some "1.0.0"
                | None -> false
              ) then
                Ok ()
              else
                Error "expected cyclic registry dependencies to terminate with exact cross-links"
          | _ -> Error "expected foo and bar to appear in the cyclic lockfile")

let test_lock_deps_handles_cyclic_local_path_dependencies = fun _ctx ->
  with_tempdir
    "riot_deps_cyclic_local_path_dep"
    (fun workspace_root ->
      let std_root = Path.(workspace_root / Path.v "vendor/std") in
      let fixme_root = Path.(workspace_root / Path.v "vendor/fixme") in
      write_package_manifest
        ~root:std_root
        {|
[package]
name = "std"
version = "0.1.0"

[build-dependencies]
fixme = { path = "../fixme" }
|};
      write_package_manifest
        ~root:fixme_root
        {|
[package]
name = "fixme"
version = "0.1.0"

[dependencies]
std = { path = "../std" }
|};
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "std" (source ~path:(Path.v "../../vendor/std") ()) ]
          ()
      in
      match run_lock_deps ~workspace_root ~mode:Refresh ~existing_lock:None [ app_pkg ] with
      | Error err ->
          Error ("expected cyclic local path dependencies to resolve: " ^ pm_error_message err)
      | Ok lockfile ->
          let std_lock =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Lockfile.package) -> has_name "std" pkg.id.name)
          in
          let fixme_lock =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Lockfile.package) -> has_name "fixme" pkg.id.name)
          in
          match (std_lock, fixme_lock) with
          | (Some std_lock, Some fixme_lock) ->
              if List.length lockfile.packages = 3
              && List.length std_lock.build_dependencies = 1
              && (
                (
                  List.head std_lock.build_dependencies
                  |> Option.expect ~msg:"expected std build dependency"
                ).package.name
                |> has_name "fixme"
              )
              && List.length fixme_lock.dependencies = 1
              && (
                (
                  List.head fixme_lock.dependencies
                  |> Option.expect ~msg:"expected fixme dependency"
                ).package.name
                |> has_name "std"
              ) then
                Ok ()
              else
                Error "expected local path dependency cycle to reuse in-flight lock nodes"
          | _ -> Error "expected std and fixme to appear in the local cyclic lockfile")

let test_lock_refresh_preserves_existing_registry_version = fun _ctx ->
  with_tempdir
    "riot_deps_refresh_preserve_registry"
    (fun workspace_root ->
      let requirement =
        Std.Version.parse_requirement "*"
        |> Result.expect ~msg:"expected requirement to parse"
      in
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "std" (source ~version:requirement ()) ]
          ()
      in
      let existing_lock =
        Riot_model.Lockfile.{
          format_version = 1;
          dependency_hash = "test";
          packages =
            [
              {
                id =
                  {
                    registry = None;
                    name = package_name "app";
                    version = None;
                    sha256 = None;
                  };
                root = Some (Path.v "packages/app");
                provenance = Workspace;
                dependencies =
                  [
                    {
                      name = package_name "std";
                      package =
                        {
                          registry = Some "pkgs.ml";
                          name = package_name "std";
                          version = Some "0.1.0";
                          sha256 = None;
                        };
                    };
                  ];
                build_dependencies = [];
                dev_dependencies = [];
              };
              {
                id =
                  {
                    registry = Some "pkgs.ml";
                    name = package_name "std";
                    version = Some "0.1.0";
                    sha256 = None;
                  };
                root = None;
                provenance = Registry { registry = "pkgs.ml" };
                dependencies = [];
                build_dependencies = [];
                dev_dependencies = [];
              };
            ];
        }
      in
      let registry =
        Pkgs_ml.Registry.in_memory
          ~cache:(make_registry_cache_at Path.(workspace_root / Path.v ".riot"))
          ~packages:[
            make_registry_document
              ~name:"std"
              ~latest:"0.2.0"
              ~releases:[ make_release ~version:"0.1.0" (); make_release ~version:"0.2.0" () ]
              ();
          ]
          ~releases:[
            make_release_source
              ~package_name:"std"
              ~version:"0.1.0"
              {|
[package]
name = "std"
version = "0.1.0"
|};
            make_release_source
              ~package_name:"std"
              ~version:"0.2.0"
              {|
[package]
name = "std"
version = "0.2.0"
|};
          ]
          ()
      in
      match run_lock_deps
        ~registry
        ~workspace_root
        ~mode:Refresh
        ~existing_lock:(Some existing_lock)
        [ app_pkg ] with
      | Error err ->
          Error ("expected refresh lock to preserve registry version: " ^ pm_error_message err)
      | Ok lockfile ->
          let app_lock =
            List.head lockfile.packages
            |> Option.expect ~msg:"expected app lock package"
          in
          if
            List.length lockfile.packages = 2
            && (
              List.head app_lock.dependencies
              |> Option.expect ~msg:"expected app dependency"
            ).package.version
            = Some "0.1.0"
            && ((
              List.get lockfile.packages ~at:1
              |> Option.expect ~msg:"expected std lock package"
            ).id.version
            = Some "0.1.0")
          then
            Ok ()
          else
            Error "expected refresh to preserve existing locked registry selections")

let test_lock_refresh_discards_stale_external_nodes = fun _ctx ->
  let app_pkg = make_package ~name:"app" ~path:(Path.v "/workspace/packages/app") () in
  let existing_lock =
    Riot_model.Lockfile.{
      format_version = 1;
      dependency_hash = "test";
      packages =
        [
          {
            id =
              {
                registry = None;
                name = package_name "app";
                version = None;
                sha256 = None;
              };
            root = Some (Path.v "packages/app");
            provenance = Workspace;
            dependencies = [];
            build_dependencies = [];
            dev_dependencies = [];
          };
          {
            id =
              {
                registry = Some "pkgs.ml";
                name = package_name "std";
                version = Some "0.1.0";
                sha256 = None;
              };
            root = None;
            provenance = Registry { registry = "pkgs.ml" };
            dependencies = [];
            build_dependencies = [];
            dev_dependencies = [];
          };
        ];
    }
  in
  match run_lock_deps ~mode:Refresh ~existing_lock:(Some existing_lock) [ app_pkg ] with
  | Error err ->
      Error ("expected refresh lock to discard stale existing nodes: " ^ pm_error_message err)
  | Ok lockfile ->
      if List.length lockfile.packages = 1
      && (
        (
          List.head lockfile.packages
          |> Option.expect ~msg:"expected app lock package"
        ).id.name
        |> has_name "app"
      ) then
        Ok ()
      else
        Error "expected refresh to discard stale external lock nodes"

let test_lock_refresh_discards_removed_workspace_packages = fun _ctx ->
  let app_pkg = make_package ~name:"app" ~path:(Path.v "/workspace/packages/app") () in
  let existing_lock =
    Riot_model.Lockfile.{
      format_version = 1;
      dependency_hash = "test";
      packages =
        [
          {
            id =
              {
                registry = None;
                name = package_name "app";
                version = None;
                sha256 = None;
              };
            root = Some (Path.v "packages/app");
            provenance = Workspace;
            dependencies = [];
            build_dependencies = [];
            dev_dependencies = [];
          };
          {
            id =
              {
                registry = None;
                name = package_name "old-app";
                version = None;
                sha256 = None;
              };
            root = Some (Path.v "packages/old-app");
            provenance = Workspace;
            dependencies = [];
            build_dependencies = [];
            dev_dependencies = [];
          };
        ];
    }
  in
  match run_lock_deps ~mode:Refresh ~existing_lock:(Some existing_lock) [ app_pkg ] with
  | Error err ->
      Error ("expected refresh lock to discard removed workspace packages: " ^ pm_error_message err)
  | Ok lockfile ->
      if List.length lockfile.packages = 1
      && (
        (
          List.head lockfile.packages
          |> Option.expect ~msg:"expected app lock package"
        ).id.name
        |> has_name "app"
      ) then
        Ok ()
      else
        Error "expected refresh to discard removed workspace packages"

let test_unlock_discards_existing_external_nodes = fun _ctx ->
  let app_pkg = make_package ~name:"app" ~path:(Path.v "/workspace/packages/app") () in
  let existing_lock =
    Riot_model.Lockfile.{
      format_version = 1;
      dependency_hash = "test";
      packages =
        [
          {
            id =
              {
                registry = Some "pkgs.ml";
                name = package_name "std";
                version = Some "0.1.0";
                sha256 = None;
              };
            root = None;
            provenance = Registry { registry = "pkgs.ml" };
            dependencies = [];
            build_dependencies = [];
            dev_dependencies = [];
          };
        ];
    }
  in
  match run_lock_deps ~mode:Unlock ~existing_lock:(Some existing_lock) [ app_pkg ] with
  | Error err -> Error ("expected unlock to rebuild workspace nodes: " ^ pm_error_message err)
  | Ok lockfile ->
      if List.length lockfile.packages = 1
      && (
        (
          List.head lockfile.packages
          |> Option.expect ~msg:"expected app lock package"
        ).id.name
        |> has_name "app"
      ) then
        Ok ()
      else
        Error "expected unlock to discard preserved external lock nodes"

let test_update_targets_only_requested_registry_packages = fun _ctx ->
  with_tempdir
    "riot_deps_update_targeted"
    (fun workspace_root ->
      let workspace_manifest = Path.(workspace_root / Path.v "riot.toml") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      write_file workspace_manifest {|
[workspace]
members = ["packages/app"]
|};
      write_package_manifest
        ~root:app_root
        {|
[package]
name = "app"
version = "0.0.1"

[dependencies]
std = "*"
mime = "*"
|};
      let registry =
        Pkgs_ml.Registry.in_memory
          ~cache:(make_registry_cache_at Path.(workspace_root / Path.v ".riot"))
          ~packages:[
            make_registry_document
              ~name:"std"
              ~latest:"0.2.0"
              ~releases:[ make_release ~version:"0.1.0" (); make_release ~version:"0.2.0" () ]
              ();
            make_registry_document
              ~name:"mime"
              ~latest:"0.2.0"
              ~releases:[ make_release ~version:"0.1.0" (); make_release ~version:"0.2.0" () ]
              ();
          ]
          ~releases:[
            make_release_source
              ~package_name:"std"
              ~version:"0.1.0"
              {|
[package]
name = "std"
version = "0.1.0"
|};
            make_release_source
              ~package_name:"std"
              ~version:"0.2.0"
              {|
[package]
name = "std"
version = "0.2.0"
|};
            make_release_source
              ~package_name:"mime"
              ~version:"0.1.0"
              {|
[package]
name = "mime"
version = "0.1.0"
|};
            make_release_source
              ~package_name:"mime"
              ~version:"0.2.0"
              {|
[package]
name = "mime"
version = "0.2.0"
|};
          ]
          ()
      in
      let lock_id name version =
        Riot_model.Lockfile.{
          registry = Some "pkgs.ml";
          name = package_name name;
          version = Some version;
          sha256 = Some "deadbeef";
        }
      in
      let app_id =
        Riot_model.Lockfile.{
          registry = None;
          name = package_name "app";
          version = None;
          sha256 = None;
        }
      in
      let existing_lock =
        Riot_model.Lockfile.{
          format_version = 1;
          dependency_hash = "old";
          packages =
            [
              {
                id = app_id;
                root = Some (Path.v "packages/app");
                provenance = Workspace;
                dependencies = [
                  { name = package_name "std"; package = lock_id "std" "0.1.0" };
                  { name = package_name "mime"; package = lock_id "mime" "0.1.0" };
                ];
                build_dependencies = [];
                dev_dependencies = [];
              };
              {
                id = lock_id "std" "0.1.0";
                root = None;
                provenance = Registry { registry = "pkgs.ml" };
                dependencies = [];
                build_dependencies = [];
                dev_dependencies = [];
              };
              {
                id = lock_id "mime" "0.1.0";
                root = None;
                provenance = Registry { registry = "pkgs.ml" };
                dependencies = [];
                build_dependencies = [];
                dev_dependencies = [];
              };
            ];
        }
      in
      Riot_deps.Lockfile_store.write ~workspace_root existing_lock
      |> Result.map_err ~fn:Riot_deps.Lockfile_store.error_message
      |> Result.and_then
        ~fn:(fun () ->
          let workspace_manager = Riot_model.Workspace_manager.create () in
          Riot_model.Workspace_manager.scan workspace_manager workspace_root
          |> Result.map_err
            ~fn:(fun err ->
              "expected workspace scan to succeed: "
              ^ Riot_model.Workspace_manager.scan_error_message err)
          |> Result.and_then
            ~fn:(fun (workspace, load_errors) ->
              if not (List.is_empty load_errors) then
                Error "expected workspace scan to have no load errors"
              else
                let events = ref [] in
                Riot_deps.update
                  ~on_event:(fun event -> events := event :: !events)
                  ~registry
                  ~workspace_manager
                  ~workspace
                  ~request:Riot_deps.{ packages = [ package_name "std" ] }
                  ()
                |> Result.map_err ~fn:Riot_deps.package_error_message
                |> Result.and_then
                  ~fn:(fun () ->
                    Riot_deps.Lockfile_store.read ~workspace_root
                    |> Result.map_err ~fn:Riot_deps.Lockfile_store.error_message
                    |> Result.and_then
                      ~fn:(fun __tmp1 ->
                        match __tmp1 with
                        | None -> Error "expected update to write riot.lock"
                        | Some (lockfile: Lockfile.t) ->
                            let std_lock =
                              List.find
                                lockfile.packages
                                ~fn:(fun (pkg: Lockfile.package) -> has_name "std" pkg.id.name)
                            in
                            let mime_lock =
                              List.find
                                lockfile.packages
                                ~fn:(fun (pkg: Lockfile.package) -> has_name "mime" pkg.id.name)
                            in
                            let updated_std =
                              List.any
                                !events
                                ~fn:(fun __tmp1 ->
                                  match __tmp1 with
                                  | Riot_model.Event.DepsPackageVersionUpdated {
                                      package;
                                      from_version;
                                      to_version;
                                    } ->
                                      has_name "std" package
                                      && String.equal from_version "0.1.0"
                                      && String.equal to_version "0.2.0"
                                  | _ -> false)
                            in
                            match (std_lock, mime_lock) with
                            | (Some std_lock, Some mime_lock) when std_lock.id.version
                            = Some "0.2.0"
                            && mime_lock.id.version = Some "0.1.0"
                            && updated_std -> Ok ()
                            | _ -> Error "expected targeted update to update std and preserve mime")))))

let test_lock_refresh_requires_lock_when_missing = fun _ctx ->
  with_tempdir
    "riot_deps_missing_lock"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "riot.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path
      |> Result.expect ~msg:"expected manifest write to succeed";
      let workspace_manager = workspace_manager () in
      match Riot_deps.Lock_refresh.needs_refresh
        ~workspace_manager
        ~workspace_root
        ~manifest_paths:[ manifest_path ]
        ~lockfile:None with
      | Ok true -> Ok ()
      | Ok false -> Error "expected missing lockfile to require refresh"
      | Error err -> Error (Riot_deps.Lock_refresh.error_message err))

let test_lock_refresh_false_when_dependency_hash_matches = fun _ctx ->
  with_tempdir
    "riot_deps_matching_dep_hash"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "riot.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path
      |> Result.expect ~msg:"expected manifest write to succeed";
      let workspace_manager = workspace_manager () in
      let dependency_hash =
        Riot_deps.Lock_refresh.dependency_hash
          ~workspace_manager
          ~workspace_root
          ~manifest_paths:[ manifest_path ]
        |> Result.expect ~msg:"expected dependency hash to compute"
      in
      let lockfile = Riot_model.Lockfile.{ format_version = 1; dependency_hash; packages = [] } in
      match Riot_deps.Lock_refresh.needs_refresh
        ~workspace_manager
        ~workspace_root
        ~manifest_paths:[ manifest_path ]
        ~lockfile:(Some lockfile) with
      | Ok false -> Ok ()
      | Ok true -> Error "expected matching dependency hash to avoid refresh"
      | Error err -> Error (Riot_deps.Lock_refresh.error_message err))

let test_lock_refresh_true_when_dependency_hash_changes = fun _ctx ->
  with_tempdir
    "riot_deps_changed_dep_hash"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "riot.toml") in
      Fs.write "[workspace]\nmembers = []\n[dependencies]\nstd = \"*\"\n" manifest_path
      |> Result.expect ~msg:"expected manifest write to succeed";
      let workspace_manager = workspace_manager () in
      let dependency_hash =
        Riot_deps.Lock_refresh.dependency_hash
          ~workspace_manager
          ~workspace_root
          ~manifest_paths:[ manifest_path ]
        |> Result.expect ~msg:"expected dependency hash to compute"
      in
      let lockfile = Riot_model.Lockfile.{ format_version = 1; dependency_hash; packages = [] } in
      Fs.write "[workspace]\nmembers = []\n[dependencies]\nstd = \"0.1.0\"\n" manifest_path
      |> Result.expect ~msg:"expected manifest rewrite to succeed";
      match Riot_deps.Lock_refresh.needs_refresh
        ~workspace_manager
        ~workspace_root
        ~manifest_paths:[ manifest_path ]
        ~lockfile:(Some lockfile) with
      | Ok true -> Ok ()
      | Ok false -> Error "expected dependency hash change to require refresh"
      | Error err -> Error (Riot_deps.Lock_refresh.error_message err))

let test_lock_refresh_reports_non_table_dependency_sections = fun _ctx ->
  with_tempdir
    "riot_deps_bad_dep_section"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "riot.toml") in
      Fs.write "dependencies = \"oops\"\n[workspace]\nmembers = []\n" manifest_path
      |> Result.expect ~msg:"expected manifest write to succeed";
      let workspace_manager = workspace_manager () in
      match Riot_deps.Lock_refresh.dependency_hash
        ~workspace_manager
        ~workspace_root
        ~manifest_paths:[ manifest_path ] with
      | Error (
        Riot_deps.Lock_refresh.DependencySectionMustBeTable { manifest_path = path; section }
      ) ->
          if Path.equal path manifest_path && String.equal section "dependencies" then
            Ok ()
          else
            Error "expected dependency-section error to preserve path and section"
      | Error err ->
          Error ("unexpected lock refresh error: " ^ Riot_deps.Lock_refresh.error_message err)
      | Ok _ -> Error "expected non-table dependency section to fail")

let test_lockfile_store_roundtrips = fun _ctx ->
  with_tempdir
    "riot_deps_lockfile_store"
    (fun workspace_root ->
      let lockfile =
        Riot_model.Lockfile.{
          format_version = 1;
          dependency_hash = "deadbeef";
          packages =
            [
              {
                id =
                  {
                    registry = None;
                    name = package_name "app";
                    version = None;
                    sha256 = None;
                  };
                root = Some (Path.v "packages/app");
                provenance = Workspace;
                dependencies = [];
                build_dependencies = [];
                dev_dependencies = [];
              };
            ];
        }
      in
      match Riot_deps.Lockfile_store.write ~workspace_root lockfile with
      | Error err ->
          Error ("expected lockfile write to succeed: " ^ Riot_deps.Lockfile_store.error_message err)
      | Ok () ->
          match Riot_deps.Lockfile_store.read ~workspace_root with
          | Error err ->
              Error ("expected lockfile read to succeed: "
              ^ Riot_deps.Lockfile_store.error_message err)
          | Ok None -> Error "expected written lockfile to exist"
          | Ok (Some reloaded) ->
              if reloaded.format_version = 1
              && String.equal reloaded.dependency_hash "deadbeef"
              && List.length reloaded.packages = 1
              && (
                (
                  List.head reloaded.packages
                  |> Option.expect ~msg:"expected reloaded app package"
                ).id.name
                |> has_name "app"
              ) then
                Ok ()
              else
                Error "expected lockfile store roundtrip to preserve package data")

let test_lockfile_store_returns_none_when_missing = fun _ctx ->
  with_tempdir
    "riot_deps_missing_store"
    (fun workspace_root ->
      match Riot_deps.Lockfile_store.read ~workspace_root with
      | Ok None -> Ok ()
      | Ok (Some _) -> Error "expected missing lockfile to return none"
      | Error err -> Error (Riot_deps.Lockfile_store.error_message err))

let test_lockfile_store_bubbles_parse_errors = fun _ctx ->
  with_tempdir
    "riot_deps_invalid_lockfile"
    (fun workspace_root ->
      let lock_path = Riot_model.Riot_dirs.package_lock_path ~workspace_root in
      Fs.write "not = [valid\n" lock_path
      |> Result.expect ~msg:"expected invalid lockfile write to succeed";
      match Riot_deps.Lockfile_store.read ~workspace_root with
      | Ok _ -> Error "expected invalid lockfile to fail"
      | Error (Riot_deps.Lockfile_store.TomlParseFailed { path; _ }) ->
          if Path.equal path lock_path then
            Ok ()
          else
            Error "expected TOML parse error to preserve lockfile path"
      | Error (Riot_deps.Lockfile_store.DecodeFailed { path; _ }) ->
          if Path.equal path lock_path then
            Ok ()
          else
            Error "expected lockfile decode error to preserve lockfile path"
      | Error err -> Error ("unexpected error: " ^ Riot_deps.Lockfile_store.error_message err))

let test_remove_reports_missing_package_dependency_when_only_inherited_from_workspace = fun _ctx ->
  with_tempdir
    "riot_deps_remove_inherited"
    (fun workspace_root ->
      let workspace_manifest = Path.(workspace_root / Path.v "riot.toml") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      write_file
        workspace_manifest
        {|
[workspace]
members = ["packages/app"]

[dependencies]
std = "*"
|};
      write_package_manifest ~root:app_root {|
[package]
name = "app"
version = "0.0.1"
|};
      let workspace =
        make_workspace_manifest
          ~workspace_root
          ~dependencies:[ dependency "std" (source ~version:Std.Version.any ()) ]
          [ make_package ~name:"app" ~path:app_root () ]
      in
      let workspace_manager = Riot_model.Workspace_manager.create () in
      match Riot_deps.remove
        ~workspace_manager
        ~workspace
        ~cwd:app_root
        ~request:Riot_deps.{
          selection = Current;
          scope = Runtime;
          dependencies = [ package_name "std" ];
        }
        () with
      | Ok () ->
          Error "expected remove to reject dependencies that are only inherited from the workspace root"
      | Error (Riot_deps.DependencyNotFoundInSection { path; section; dependency }) ->
          if
            Path.equal path Path.(app_root / Path.v "riot.toml")
            && String.equal section "dependencies"
            && String.equal dependency "std"
          then
            Ok ()
          else
            Error "unexpected dependency-not-found payload for inherited dependency removal"
      | Error err -> Error ("unexpected remove error: " ^ Riot_deps.package_error_message err))

let test_remove_reports_typed_manifest_update_errors = fun _ctx ->
  with_tempdir
    "riot_deps_remove_bad_manifest"
    (fun workspace_root ->
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      let app_manifest = Path.(app_root / Path.v "riot.toml") in
      write_file
        app_manifest
        {|
dependencies = "not-a-table"

[package]
name = "app"
version = "0.0.1"
|};
      let workspace =
        make_workspace_manifest
          ~workspace_root
          [
            make_package
              ~name:"app"
              ~path:app_root
              ~dependencies:[ dependency "std" (source ~version:Std.Version.any ()) ]
              ();
          ]
      in
      let workspace_manager = Riot_model.Workspace_manager.create () in
      match Riot_deps.remove
        ~workspace_manager
        ~workspace
        ~cwd:app_root
        ~request:Riot_deps.{
          selection = Current;
          scope = Runtime;
          dependencies = [ package_name "std" ];
        }
        () with
      | Ok () -> Error "expected remove to report typed manifest update error"
      | Error (
        Riot_deps.ManifestUpdateFailed (
          Riot_deps.Manifest_edit.DependencySectionMustBeTable { path; section }
        )
      ) ->
          if Path.equal path app_manifest && String.equal section "dependencies" then
            Ok ()
          else
            Error "unexpected manifest update payload for non-table dependency section"
      | Error err -> Error ("unexpected remove error: " ^ Riot_deps.package_error_message err))

let test_remove_multiple_dependencies_refreshes_lock_once = fun _ctx ->
  with_tempdir
    "riot_deps_remove_multiple"
    (fun workspace_root ->
      let workspace_manifest = Path.(workspace_root / Path.v "riot.toml") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      let widgets_root = Path.(workspace_root / Path.v "packages/widgets") in
      let gadgets_root = Path.(workspace_root / Path.v "packages/gadgets") in
      write_file workspace_manifest {|
[workspace]
members = ["packages/app"]
|};
      write_package_manifest
        ~root:app_root
        {|
[package]
name = "app"
version = "0.0.1"

[dependencies]
widgets = { path = "../widgets" }
gadgets = { path = "../gadgets" }
|};
      write_package_manifest ~root:widgets_root {|
[package]
name = "widgets"
version = "0.0.1"
|};
      write_package_manifest ~root:gadgets_root {|
[package]
name = "gadgets"
version = "0.0.1"
|};
      let workspace_manager = Riot_model.Workspace_manager.create () in
      Riot_model.Workspace_manager.scan workspace_manager workspace_root
      |> Result.map_err
        ~fn:(fun err ->
          "expected workspace scan to succeed: "
          ^ Riot_model.Workspace_manager.scan_error_message err)
      |> Result.and_then
        ~fn:(fun (workspace, load_errors) ->
          if not (List.is_empty load_errors) then
            Error "expected workspace scan to have no load errors"
          else
            Riot_deps.remove
              ~workspace_manager
              ~workspace
              ~cwd:app_root
              ~request:Riot_deps.{
                selection = Current;
                scope = Runtime;
                dependencies = [ package_name "widgets"; package_name "gadgets" ];
              }
              ()
            |> Result.map_err ~fn:Riot_deps.package_error_message
            |> Result.and_then
              ~fn:(fun () ->
                Fs.read_to_string Path.(app_root / Path.v "riot.toml")
                |> Result.map_err ~fn:IO.error_message
                |> Result.and_then
                  ~fn:(fun manifest_source ->
                    Riot_deps.Lockfile_store.read ~workspace_root
                    |> Result.map_err
                      ~fn:(fun err ->
                        "expected lockfile read to succeed: "
                        ^ Riot_deps.Lockfile_store.error_message err)
                    |> Result.and_then
                      ~fn:(fun maybe_lockfile ->
                        match maybe_lockfile with
                        | None -> Error "expected remove to rewrite riot.lock"
                        | Some (lockfile: Lockfile.t) ->
                            if
                              (not (String.contains manifest_source "widgets"))
                              && (not (String.contains manifest_source "gadgets"))
                              && List.all
                                lockfile.packages
                                ~fn:(fun (pkg: Lockfile.package) ->
                                  not (has_name "widgets" pkg.id.name)
                                  && not (has_name "gadgets" pkg.id.name))
                            then
                              Ok ()
                            else
                              Error "expected multi-remove to remove both deps from manifest and lock")))))

let test_add_path_dependency_discovers_package_name_and_refreshes_lock = fun _ctx ->
  with_tempdir
    "riot_deps_add_path"
    (fun workspace_root ->
      let workspace_manifest = Path.(workspace_root / Path.v "riot.toml") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      let lib_root = Path.(workspace_root / Path.v "packages/lib") in
      write_file workspace_manifest {|
[workspace]
members = ["packages/app"]
|};
      write_package_manifest ~root:app_root {|
[package]
name = "app"
version = "0.0.1"
|};
      write_package_manifest ~root:lib_root {|
[package]
name = "widgets"
version = "0.0.1"
|};
      let workspace_manager = Riot_model.Workspace_manager.create () in
      Riot_model.Workspace_manager.scan workspace_manager workspace_root
      |> Result.map_err
        ~fn:(fun err ->
          "expected workspace scan to succeed: "
          ^ Riot_model.Workspace_manager.scan_error_message err)
      |> Result.and_then
        ~fn:(fun (workspace, load_errors) ->
          if not (List.is_empty load_errors) then
            Error "expected workspace scan to have no load errors"
          else
            Riot_deps.add
              ~workspace_manager
              ~workspace
              ~cwd:app_root
              ~request:Riot_deps.{
                selection = Current;
                scope = Runtime;
                dependencies = [ "../lib" ];
              }
              ()
            |> Result.map_err ~fn:Riot_deps.package_error_message
            |> Result.and_then
              ~fn:(fun () ->
                Fs.read_to_string Path.(app_root / Path.v "riot.toml")
                |> Result.map_err ~fn:IO.error_message
                |> Result.and_then
                  ~fn:(fun manifest_source ->
                    Riot_deps.Lockfile_store.read ~workspace_root
                    |> Result.map_err
                      ~fn:(fun err ->
                        "expected lockfile read to succeed: "
                        ^ Riot_deps.Lockfile_store.error_message err)
                    |> Result.and_then
                      ~fn:(fun maybe_lockfile ->
                        match maybe_lockfile with
                        | None -> Error "expected add to rewrite riot.lock"
                        | Some (lockfile: Riot_model.Lockfile.t) ->
                            let app_lock =
                              List.find
                                lockfile.packages
                                ~fn:(fun (pkg: Lockfile.package) -> has_name "app" pkg.id.name)
                            in
                            if
                              String.contains manifest_source "widgets = { path = \"../lib\" }"
                              && Option.is_some app_lock
                              && List.any
                                lockfile.packages
                                ~fn:(fun (pkg: Lockfile.package) -> has_name "widgets" pkg.id.name)
                            then
                              Ok ()
                            else
                              Error "expected path add to write discovered package name and refresh riot.lock")))))

let test_add_multiple_path_dependencies_refreshes_lock_once = fun _ctx ->
  with_tempdir
    "riot_deps_add_multiple_paths"
    (fun workspace_root ->
      let workspace_manifest = Path.(workspace_root / Path.v "riot.toml") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      let widgets_root = Path.(workspace_root / Path.v "packages/widgets") in
      let gadgets_root = Path.(workspace_root / Path.v "packages/gadgets") in
      write_file workspace_manifest {|
[workspace]
members = ["packages/app"]
|};
      write_package_manifest ~root:app_root {|
[package]
name = "app"
version = "0.0.1"
|};
      write_package_manifest ~root:widgets_root {|
[package]
name = "widgets"
version = "0.0.1"
|};
      write_package_manifest ~root:gadgets_root {|
[package]
name = "gadgets"
version = "0.0.1"
|};
      let workspace_manager = Riot_model.Workspace_manager.create () in
      Riot_model.Workspace_manager.scan workspace_manager workspace_root
      |> Result.map_err
        ~fn:(fun err ->
          "expected workspace scan to succeed: "
          ^ Riot_model.Workspace_manager.scan_error_message err)
      |> Result.and_then
        ~fn:(fun (workspace, load_errors) ->
          if not (List.is_empty load_errors) then
            Error "expected workspace scan to have no load errors"
          else
            Riot_deps.add
              ~workspace_manager
              ~workspace
              ~cwd:app_root
              ~request:Riot_deps.{
                selection = Current;
                scope = Runtime;
                dependencies = [ "../widgets"; "../gadgets" ];
              }
              ()
            |> Result.map_err ~fn:Riot_deps.package_error_message
            |> Result.and_then
              ~fn:(fun () ->
                Fs.read_to_string Path.(app_root / Path.v "riot.toml")
                |> Result.map_err ~fn:IO.error_message
                |> Result.and_then
                  ~fn:(fun manifest_source ->
                    Riot_deps.Lockfile_store.read ~workspace_root
                    |> Result.map_err
                      ~fn:(fun err ->
                        "expected lockfile read to succeed: "
                        ^ Riot_deps.Lockfile_store.error_message err)
                    |> Result.and_then
                      ~fn:(fun maybe_lockfile ->
                        match maybe_lockfile with
                        | None -> Error "expected add to rewrite riot.lock"
                        | Some (lockfile: Lockfile.t) ->
                            if
                              String.contains manifest_source "widgets = { path = \"../widgets\" }"
                              && String.contains
                                manifest_source
                                "gadgets = { path = \"../gadgets\" }"
                              && List.any
                                lockfile.packages
                                ~fn:(fun (pkg: Lockfile.package) -> has_name "widgets" pkg.id.name)
                              && List.any
                                lockfile.packages
                                ~fn:(fun (pkg: Lockfile.package) -> has_name "gadgets" pkg.id.name)
                            then
                              Ok ()
                            else
                              Error "expected multi-add to write both discovered package names and refresh riot.lock")))))

let test_add_path_dependency_reports_missing_manifest = fun _ctx ->
  with_tempdir
    "riot_deps_add_missing_path"
    (fun workspace_root ->
      let workspace_manifest = Path.(workspace_root / Path.v "riot.toml") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      write_file workspace_manifest {|
[workspace]
members = ["packages/app"]
|};
      write_package_manifest ~root:app_root {|
[package]
name = "app"
version = "0.0.1"
|};
      let workspace_manager = Riot_model.Workspace_manager.create () in
      Riot_model.Workspace_manager.scan workspace_manager workspace_root
      |> Result.map_err
        ~fn:(fun err ->
          "expected workspace scan to succeed: "
          ^ Riot_model.Workspace_manager.scan_error_message err)
      |> Result.and_then
        ~fn:(fun (workspace, load_errors) ->
          if not (List.is_empty load_errors) then
            Error "expected workspace scan to have no load errors"
          else
            match Riot_deps.add
              ~workspace_manager
              ~workspace
              ~cwd:app_root
              ~request:Riot_deps.{
                selection = Current;
                scope = Runtime;
                dependencies = [ "../missing" ];
              }
              () with
            | Error (
              Riot_deps.PathDependencyLoadFailed {
                dependency;
                path;
                error = Riot_deps.PathDependencyManifestReadFailed _;
              }
            ) ->
                if
                  String.equal dependency "../missing"
                  && Path.equal path Path.(workspace_root / Path.v "packages/missing")
                then
                  Ok ()
                else
                  Error "unexpected missing path dependency payload"
            | Error err -> Error ("unexpected add error: " ^ Riot_deps.package_error_message err)
            | Ok () -> Error "expected missing path dependency manifest to fail"))

let test_git_dependency_parse_spec_normalizes_github_source = fun _ctx ->
  match Riot_deps.Git_dependency.parse_spec "https://github.com/riot-tests/widgets-add#main" with
  | Ok { source_locator; ref_ } ->
      if String.equal source_locator "github.com/riot-tests/widgets-add" && ref_ = Some "main" then
        Ok ()
      else
        Error "expected github source spec to normalize into locator + ref"
  | Error err ->
      Error ("expected git dependency spec to parse: " ^ Riot_deps.Git_dependency.message err)

let test_git_dependency_parse_spec_reports_multiple_ref_suffixes = fun _ctx ->
  match Riot_deps.Git_dependency.parse_spec "github.com/riot/tests#main#extra" with
  | Error (
    Riot_deps.Git_dependency.InvalidSourceSpec {
      source;
      reason = Riot_deps.Git_dependency.TooManyRefSuffixes;
    }
  ) ->
      if String.equal source "github.com/riot/tests#main#extra" then
        Ok ()
      else
        Error "expected invalid source spec to preserve raw source"
  | Error err ->
      Error ("expected multiple ref suffix error, got: " ^ Riot_deps.Git_dependency.message err)
  | Ok _ -> Error "expected source spec with multiple ref suffixes to fail"

let test_git_dependency_parse_source_locator_reports_invalid_shape = fun _ctx ->
  match Riot_deps.Git_dependency.parse_source_locator "github.com/riot" with
  | Error (
    Riot_deps.Git_dependency.InvalidSourceSpec {
      source;
      reason = Riot_deps.Git_dependency.InvalidLocatorShape;
    }
  ) ->
      if String.equal source "github.com/riot" then
        Ok ()
      else
        Error "expected invalid locator shape to preserve raw source"
  | Error err ->
      Error ("expected invalid locator shape error, got: " ^ Riot_deps.Git_dependency.message err)
  | Ok _ -> Error "expected incomplete source locator to fail"

let test_git_dependency_sync_checkout_clones_local_repo = fun _ctx ->
  with_tempdir
    "riot_deps_git_checkout"
    (fun root ->
      let origin = Path.(root / Path.v "origin") in
      let checkout = Path.(root / Path.v "checkout") in
      prepare_local_git_repo ~root:origin ~package_name:"widgets" ()
      |> Result.and_then
        ~fn:(fun _repo_root ->
          Riot_deps.Git_dependency.sync_checkout
            ~repo_dir:checkout
            ~remote_url:(Path.to_string origin)
            ~ref_:"main"
            ()
          |> Result.map_err ~fn:Riot_deps.Git_dependency.message
          |> Result.and_then
            ~fn:(fun _ ->
              Fs.read_to_string Path.(checkout / Path.v "riot.toml")
              |> Result.map_err ~fn:IO.error_message
              |> Result.and_then
                ~fn:(fun manifest_source ->
                  if String.contains manifest_source "name = \"widgets\"" then
                    Ok ()
                  else
                    Error "expected git dependency checkout to clone the local repository"))))

let test_git_dependency_sync_checkout_skips_fetch_without_update = fun _ctx ->
  with_tempdir
    "riot_deps_git_checkout_no_update"
    (fun root ->
      let origin = Path.(root / Path.v "origin") in
      let checkout = Path.(root / Path.v "checkout") in
      prepare_local_git_repo ~root:origin ~package_name:"widgets" ()
      |> Result.and_then
        ~fn:(fun _repo_root ->
          Riot_deps.Git_dependency.sync_checkout
            ~repo_dir:checkout
            ~remote_url:(Path.to_string origin)
            ~ref_:"main"
            ()
          |> Result.map_err ~fn:Riot_deps.Git_dependency.message
          |> Result.and_then
            ~fn:(fun _ ->
              write_file
                Path.(origin / Path.v "riot.toml")
                {|
[package]
name = "widgets-next"
version = "0.0.2"
description = "widgets-next"
license = "Apache-2.0"
public = true
|};
              run_git_steps ~cwd:origin [ [ "add"; "." ]; [ "commit"; "-m"; "update" ] ]
              |> Result.and_then
                ~fn:(fun _ ->
                  Riot_deps.Git_dependency.sync_checkout
                    ~update:false
                    ~repo_dir:checkout
                    ~remote_url:(Path.to_string origin)
                    ~ref_:"main"
                    ()
                  |> Result.map_err ~fn:Riot_deps.Git_dependency.message
                  |> Result.and_then
                    ~fn:(fun _ ->
                      Fs.read_to_string Path.(checkout / Path.v "riot.toml")
                      |> Result.map_err ~fn:IO.error_message
                      |> Result.and_then
                        ~fn:(fun manifest_source ->
                          if String.contains manifest_source "name = \"widgets\"" then
                            Ok ()
                          else if String.contains manifest_source "name = \"widgets-next\"" then
                            Error "expected sync_checkout ~update:false to keep the cached checkout without fetching upstream changes"
                          else
                            Error "expected cached checkout to preserve the original manifest contents"))))))

let test_add_rejects_unsupported_source_dependency_specs = fun _ctx ->
  with_tempdir
    "riot_deps_add_source_invalid"
    (fun workspace_root ->
      let workspace_manifest = Path.(workspace_root / Path.v "riot.toml") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      write_file workspace_manifest {|
[workspace]
members = ["packages/app"]
|};
      write_package_manifest ~root:app_root {|
[package]
name = "app"
version = "0.0.1"
|};
      let workspace_manager = Riot_model.Workspace_manager.create () in
      Riot_model.Workspace_manager.scan workspace_manager workspace_root
      |> Result.map_err
        ~fn:(fun err ->
          "expected workspace scan to succeed: "
          ^ Riot_model.Workspace_manager.scan_error_message err)
      |> Result.and_then
        ~fn:(fun (workspace, load_errors) ->
          if not (List.is_empty load_errors) then
            Error "expected workspace scan to have no load errors"
          else
            match Riot_deps.add
              ~workspace_manager
              ~workspace
              ~cwd:app_root
              ~request:Riot_deps.{
                selection = Current;
                scope = Runtime;
                dependencies = [ "https://gitlab.com/leostera/widgets" ];
              }
              () with
            | Ok () -> Error "expected unsupported non-github source dependency add to fail"
            | Error (
              Riot_deps.DependencySpecInvalid {
                dependency;
                error =
                  Riot_deps.SourceDependencySpecError (
                    Riot_deps.Git_dependency.UnsupportedSourceHost { host; _ }
                  );
              }
            ) ->
                if
                  String.equal dependency "https://gitlab.com/leostera/widgets"
                  && String.equal host "gitlab.com"
                then
                  Ok ()
                else
                  Error "unexpected unsupported source dependency payload"
            | Error err -> Error ("unexpected add error: " ^ Riot_deps.package_error_message err)))

let test_package_error_message_renders_typed_source_dependency_errors = fun _ctx ->
  let message_for error =
    Riot_deps.package_error_message
      (
        Riot_deps.SourceDependencyLoadFailed {
          dependency = "github.com/riot-tests/widgets";
          source_locator = "github.com/riot-tests/widgets";
          ref_ = Some "main";
          error;
        }
      )
  in
  let cases = [
    (
      Riot_deps.SourceDependencyMaterializationFailed (Riot_deps.Git_dependency.PackageRootMissing {
        path = Path.v "/cache/widgets";
      }),
      "materialized source dependency is missing package root"
    );
    (Riot_deps.SourceDependencyManifestReadFailed (IO.Unknown_error "read boom"), "read boom");
    (
      Riot_deps.SourceDependencyTomlParseFailed (Data.Toml.Parse_error {
        position = 7;
        context = "package";
        reason = "bad toml";
      }),
      "bad toml"
    );
    (
      Riot_deps.SourceDependencyManifestDecodeFailed Package.ManifestMustBeTable,
      "package manifest must be a table"
    );
  ]
  in
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok ()
    | (error, expected) :: rest ->
        let message = message_for error in
        if
          String.contains message "failed to load source dependency 'github.com/riot-tests/widgets'"
          && String.contains message expected
        then
          loop rest
        else
          Error ("expected source dependency error message to include '"
          ^ expected
          ^ "', got: "
          ^ message)
  in
  loop cases

let test_package_error_message_renders_typed_workspace_reload_errors = fun _ctx ->
  let workspace_root = Path.v "/workspace" in
  let scan_message =
    Riot_deps.package_error_message
      (Riot_deps.WorkspaceReloadFailed {
        workspace_root;
        error = Riot_model.Workspace_manager.NoWorkspaceRootFound;
      })
  in
  let load_message =
    Riot_deps.package_error_message
      (Riot_deps.WorkspaceReloadHadErrors {
        workspace_root;
        errors = [
          Riot_model.Workspace_manager.PackageTomlParseFailed {
            package = "app";
            path = "/workspace/packages/app";
          };
        ];
      })
  in
  if
    String.contains scan_message "no workspace root found"
    && String.contains load_message "package 'app': failed to parse riot.toml"
  then
    Ok ()
  else
    Error "expected typed workspace reload errors to render through package_error_message"

let test_package_error_message_renders_typed_registry_initialization_errors = fun _ctx ->
  let message =
    Riot_deps.package_error_message
      (Riot_deps.RegistryInitializationFailed {
        registry = "pkgs.ml";
        error = Riot_deps.RegistryFilesystemInitializationFailed Pkgs_ml.Registry_cache.HomeDirectoryUnavailable;
      })
  in
  if
    String.contains message "failed to initialize registry 'pkgs.ml'"
    && String.contains message "failed to determine home directory for pkgs.ml cache"
  then
    Ok ()
  else
    Error "expected typed registry initialization errors to render through package_error_message"

let test_package_error_message_renders_typed_registry_operation_errors = fun _ctx ->
  let lookup_message =
    Riot_deps.package_error_message
      (Riot_deps.RegistryLookupFailed {
        package = "widgets";
        registry = "pkgs.ml";
        error = Riot_deps.RegistryPackageDocumentReadFailed "lookup failed";
      })
  in
  let search_message =
    Riot_deps.package_error_message
      (Riot_deps.RegistrySearchFailed {
        query = "widg";
        registry = "pkgs.ml";
        error = Riot_deps.RegistrySearchRequestFailed "search failed";
      })
  in
  let materialization_message =
    Riot_deps.package_error_message
      (
        Riot_deps.RegistryMaterializationFailed {
          package = "widgets";
          version = "0.1.0";
          registry = "pkgs.ml";
          error = Riot_deps.RegistryPackageManifestDecodeFailed Package.ManifestMustBeTable;
        }
      )
  in
  if
    String.contains lookup_message "lookup failed"
    && String.contains search_message "search failed"
    && String.contains materialization_message "package manifest must be a table"
  then
    Ok ()
  else
    Error "expected typed registry operation errors to render through package_error_message"

let test_package_error_message_lists_search_suggestions = fun _ctx ->
  let message =
    Riot_deps.package_error_message
      (Riot_deps.RegistryPackageNotFound {
        package = "kernl";
        registry = "pkgs.ml";
        suggestions = [
          {
            Riot_deps.package = "kernel";
            latest_version = "0.0.1";
            description = Some "Core primitives";
          };
          { Riot_deps.package = "kernel-tools"; latest_version = "0.1.0"; description = None };
        ];
      })
  in
  if
    String.contains message "package 'kernl' was not found in registry 'pkgs.ml'"
    && String.contains message "Did you mean:"
    && String.contains message "kernel@0.0.1 - Core primitives"
    && String.contains message "kernel-tools@0.1.0"
  then
    Ok ()
  else
    Error ("unexpected suggestion message:\n" ^ message)

let test_search_returns_registry_results = fun _ctx ->
  let release = {
    (make_release ~version:"0.0.1" ()) with
    description = Some "Bootstrap build tool for the Riot toolchain";
  }
  in
  let registry =
    make_registry
      [
        make_registry_document ~name:"miniriot" ~latest:"0.0.1" ~releases:[ release ] ();
        make_registry_document
          ~name:"jsonrpc"
          ~latest:"0.1.0"
          ~releases:[ make_release ~version:"0.1.0" () ]
          ();
      ]
  in
  match Riot_deps.search ~registry ~request:Riot_deps.{ query = "mini"; limit = 5 } () with
  | Error err -> Error ("expected search to succeed: " ^ Riot_deps.package_error_message err)
  | Ok [ result ] ->
      if
        String.equal result.package "miniriot"
        && String.equal result.latest_version "0.0.1"
        && Option.equal
          result.description
          (Some "Bootstrap build tool for the Riot toolchain")
          ~fn:String.equal
      then
        Ok ()
      else
        Error "unexpected search result payload"
  | Ok results -> Error ("expected one search result, got " ^ Int.to_string (List.length results))

let test_ensure_lock_refreshes_missing_lock_and_resolves_workspace = fun _ctx ->
  with_tempdir
    "riot_deps_ensure_lock_missing"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "riot.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path
      |> Result.expect ~msg:"expected workspace manifest to be written";
      write_file
        Path.(workspace_root / Path.v "packages/std/riot.toml")
        "[package]\nname = \"std\"\n";
      write_file
        Path.(workspace_root / Path.v "packages/app/riot.toml")
        "[package]\nname = \"app\"\n";
      let std_pkg =
        make_package ~name:"std" ~path:Path.(workspace_root / Path.v "packages/std") ()
      in
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "std" (source ~workspace:true ()) ]
          ()
      in
      match collect_event_names
        (fun emit ->
          ensure_lock
            ~emit
            ~registry:(make_registry [])
            ~workspace_root
            [ app_pkg; std_pkg ]) with
      | Error err -> Error ("expected ensure_lock to refresh missing lock: " ^ pm_error_message err)
      | Ok ((lockfile, resolved), event_names) ->
          let lock_path = Riot_model.Riot_dirs.package_lock_path ~workspace_root in
          if
            List.length lockfile.packages = 2
            && List.length resolved = 2
            && List.contains event_names ~value:"riot.deps.resolution.started"
            && List.contains event_names ~value:"riot.deps.resolution.refreshing_lock"
            && List.contains event_names ~value:"riot.deps.lockfile.write.started"
            && List.contains event_names ~value:"riot.deps.lockfile.write.finished"
            && List.contains event_names ~value:"riot.deps.resolution.finished"
            && Result.unwrap_or ~default:false (Fs.exists lock_path)
          then
            Ok ()
          else
            Error "expected ensure_lock to write a fresh lockfile and emit PM lifecycle events")

let test_ensure_lock_uses_existing_fresh_lock = fun _ctx ->
  with_tempdir
    "riot_deps_ensure_lock_existing"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "riot.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path
      |> Result.expect ~msg:"expected workspace manifest to be written";
      write_file
        Path.(workspace_root / Path.v "packages/std/riot.toml")
        "[package]\nname = \"std\"\n";
      write_file
        Path.(workspace_root / Path.v "packages/app/riot.toml")
        "[package]\nname = \"app\"\n";
      let std_pkg =
        make_package ~name:"std" ~path:Path.(workspace_root / Path.v "packages/std") ()
      in
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "std" (source ~workspace:true ()) ]
          ()
      in
      let existing_lock =
        run_lock_deps ~workspace_root ~mode:Refresh ~existing_lock:None [ app_pkg; std_pkg ]
        |> Result.expect ~msg:"expected workspace lock projection to succeed"
      in
      let workspace_manager = workspace_manager () in
      let dependency_hash =
        Riot_deps.Lock_refresh.dependency_hash
          ~workspace_manager
          ~workspace_root
          ~manifest_paths:[
            manifest_path;
            Path.(workspace_root / Path.v "packages/std/riot.toml");
            Path.(workspace_root / Path.v "packages/app/riot.toml");
          ]
        |> Result.expect ~msg:"expected dependency hash to compute"
      in
      let existing_lock = { existing_lock with dependency_hash } in
      Riot_deps.Lockfile_store.write ~workspace_root existing_lock
      |> Result.expect ~msg:"expected initial lockfile to be written";
      match collect_event_names
        (fun emit ->
          ensure_lock
            ~emit
            ~registry:(make_registry [])
            ~workspace_root
            [ app_pkg; std_pkg ]) with
      | Error err -> Error ("expected ensure_lock to use existing lock: " ^ pm_error_message err)
      | Ok ((lockfile, resolved), event_names) ->
          if
            List.length lockfile.packages = 2
            && List.length resolved = 2
            && List.contains event_names ~value:"riot.deps.resolution.using_existing_lock"
            && not (List.contains event_names ~value:"riot.deps.lockfile.write.started")
            && not (List.contains event_names ~value:"riot.deps.resolution.finished")
          then
            Ok ()
          else
            Error "expected ensure_lock to reuse a fresh existing lock without rewriting it")

let test_ensure_lock_materializes_registry_packages_during_projection = fun _ctx ->
  with_tempdir
    "riot_deps_ensure_lock_materializes"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "riot.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path
      |> Result.expect ~msg:"expected workspace manifest to be written";
      write_file
        Path.(workspace_root / Path.v "packages/app/riot.toml")
        "[package]\nname = \"app\"\n";
      let requirement =
        Std.Version.parse_requirement "*"
        |> Result.expect ~msg:"expected requirement to parse"
      in
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "std" (source ~version:requirement ()) ]
          ()
      in
      let registry_cache =
        Pkgs_ml.Registry_cache.create
          ~riot_home:Path.(workspace_root / Path.v ".riot")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to initialize"
      in
      let registry =
        Pkgs_ml.Registry.in_memory
          ~cache:registry_cache
          ~packages:[
            make_registry_document
              ~name:"std"
              ~latest:"0.2.0"
              ~releases:[ make_release ~version:"0.2.0" () ]
              ();
          ]
          ~releases:[
            {
              Pkgs_ml.Registry.package_name = "std";
              version = "0.2.0";
              manifest_toml = "[package]\nname = \"std\"\n";
              files = [ { path = Path.v "src/std.ml"; contents = "let answer = 42\n" } ];
            };
          ]
          ()
      in
      match collect_event_names
        (fun emit ->
          ensure_lock ~emit ~registry ~workspace_root [ app_pkg ]) with
      | Error err ->
          Error ("expected ensure_lock to materialize registry packages: " ^ pm_error_message err)
      | Ok ((_, resolved), event_names) ->
          let manifest_path =
            Pkgs_ml.Registry_cache.package_src_dir
              registry_cache
              ~package_name:"std"
              ~version:"0.2.0"
            |> fun root -> Path.(root / Path.v "riot.toml")
          in
          if List.length resolved = 2
          && Result.unwrap_or ~default:false (Fs.exists manifest_path)
          && List.contains event_names ~value:"riot.deps.universe.building"
          && List.contains event_names ~value:"riot.deps.universe.built"
          && List.contains event_names ~value:"riot.deps.package.metadata.fetch.started"
          && List.contains event_names ~value:"riot.deps.package.metadata.fetch.finished"
          && List.contains event_names ~value:"riot.deps.package.version.locked"
          && List.contains event_names ~value:"riot.deps.package.materialization.started"
          && List.contains event_names ~value:"riot.deps.package.materialization.finished"
          && List.contains event_names ~value:"riot.deps.package.manifest.fetch.started"
          && List.contains event_names ~value:"riot.deps.package.manifest.fetch.finished"
          && List.contains event_names ~value:"riot.deps.package.resolved_for_build" then
            Ok ()
          else
            Error "expected ensure_lock to lazily materialize external package manifests during projection")

let test_ensure_lock_reuses_existing_lock_and_repairs_missing_registry_packages = fun _ctx ->
  with_tempdir
    "riot_deps_ensure_lock_materializes_existing"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "riot.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path
      |> Result.expect ~msg:"expected workspace manifest to be written";
      write_file
        Path.(workspace_root / Path.v "packages/app/riot.toml")
        "[package]\nname = \"app\"\n";
      let requirement =
        Std.Version.parse_requirement "*"
        |> Result.expect ~msg:"expected requirement to parse"
      in
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "std" (source ~version:requirement ()) ]
          ()
      in
      let registry_cache =
        Pkgs_ml.Registry_cache.create
          ~riot_home:Path.(workspace_root / Path.v ".riot")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to initialize"
      in
      let registry =
        Pkgs_ml.Registry.in_memory
          ~cache:registry_cache
          ~packages:[
            make_registry_document
              ~name:"std"
              ~latest:"0.2.0"
              ~releases:[ make_release ~version:"0.2.0" () ]
              ();
          ]
          ~releases:[
            {
              Pkgs_ml.Registry.package_name = "std";
              version = "0.2.0";
              manifest_toml = "[package]\nname = \"std\"\n";
              files = [];
            };
          ]
          ()
      in
      let existing_lock =
        Riot_deps.Dep_solver.lock_deps
          ~mode:Riot_deps.Dep_solver.Refresh
          ~registry
          ~existing_lock:None
          ~workspace:(make_workspace_manifest ~workspace_root [ app_pkg ])
          ()
        |> Result.expect ~msg:"expected initial lock solve to succeed"
      in
      let existing_lock = {
        existing_lock with
        dependency_hash =
          Riot_deps.Lock_refresh.dependency_hash
            ~workspace_manager:(workspace_manager ())
            ~workspace_root
            ~manifest_paths:[
              manifest_path;
              Path.(workspace_root / Path.v "packages/app/riot.toml");
            ]
          |> Result.expect ~msg:"expected dependency hash to compute";
      }
      in
      Riot_deps.Lockfile_store.write ~workspace_root existing_lock
      |> Result.expect ~msg:"expected initial lockfile write to succeed";
      let materialized_std_root =
        Pkgs_ml.Registry_cache.package_src_dir registry_cache ~package_name:"std" ~version:"0.2.0"
      in
      Fs.remove_dir_all materialized_std_root
      |> Result.expect ~msg:"expected materialized registry package to be removed";
      match collect_event_names
        (fun emit ->
          ensure_lock ~emit ~registry ~workspace_root [ app_pkg ]) with
      | Error err ->
          Error ("expected ensure_lock to reuse lock and materialize missing packages: "
          ^ pm_error_message err)
      | Ok ((_, resolved), event_names) ->
          if
            List.length resolved = 2
            && List.contains event_names ~value:"riot.deps.resolution.using_existing_lock"
            && not (List.contains event_names ~value:"riot.deps.resolution.refreshing_lock")
            && Result.unwrap_or
              ~default:false
              (Fs.exists Path.(materialized_std_root / Path.v "riot.toml"))
            && List.contains event_names ~value:"riot.deps.package.materialization.finished"
            && not (List.contains event_names ~value:"riot.deps.lockfile.write.started")
          then
            Ok ()
          else
            Error "expected ensure_lock to reuse the lock while still materializing missing registry packages")

let test_ensure_workspace_projects_materialized_registry_packages = fun _ctx ->
  with_tempdir
    "riot_deps_ensure_workspace"
    (fun workspace_root ->
      let workspace_manifest = Path.(workspace_root / Path.v "riot.toml") in
      Fs.write "[workspace]\nmembers = [\"packages/app\"]\n" workspace_manifest
      |> Result.expect ~msg:"expected workspace manifest to be written";
      write_file
        Path.(workspace_root / Path.v "packages/app/riot.toml")
        "[package]\nname = \"app\"\n";
      let registry_cache =
        Pkgs_ml.Registry_cache.create
          ~riot_home:Path.(workspace_root / Path.v ".riot")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to initialize"
      in
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "std" (source ~version:Std.Version.any ()) ]
          ()
      in
      let app_pkg =
        Riot_model.Package.make
          ~name:app_pkg.name
          ~path:app_pkg.path
          ~relative_path:(Path.v "packages/app")
          ~dependencies:app_pkg.dependencies
          ~dev_dependencies:app_pkg.dev_dependencies
          ~build_dependencies:app_pkg.build_dependencies
          ~foreign_dependencies:app_pkg.foreign_dependencies
          ~binaries:app_pkg.binaries
          ?library:app_pkg.library
          ~sources:app_pkg.sources
          ~compiler:app_pkg.compiler
          ~commands:app_pkg.commands
          ~fix_providers:app_pkg.fix_providers
          ~publish:app_pkg.publish
          ()
      in
      let workspace =
        Riot_model.Workspace_manifest.make_realized ~root:workspace_root ~packages:[ app_pkg ] ()
      in
      let registry =
        Pkgs_ml.Registry.in_memory
          ~cache:registry_cache
          ~packages:[
            make_registry_document
              ~name:"std"
              ~latest:"0.2.0"
              ~releases:[ make_release ~version:"0.2.0" () ]
              ();
          ]
          ~releases:[
            {
              Pkgs_ml.Registry.package_name = "std";
              version = "0.2.0";
              manifest_toml = {|
[package]
name = "std"
version = "0.2.0"
|};
              files = [];
            };
          ]
          ()
      in
      let workspace_manager = workspace_manager () in
      match Riot_deps.ensure_workspace
        ~workspace_manager
        ~mode:Riot_deps.Dep_solver.Refresh
        ~registry
        ~workspace
        () with
      | Error err -> Error ("expected ensure_workspace to succeed: " ^ pm_error_message err)
      | Ok resolved_workspace ->
          let std_pkg =
            List.find
              resolved_workspace.packages
              ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> has_name "std" pkg.name)
          in
          let expected_std_root =
            Pkgs_ml.Registry_cache.package_src_dir
              registry_cache
              ~package_name:"std"
              ~version:"0.2.0"
          in
          match std_pkg with
          | Some std_pkg ->
              if
                List.map resolved_workspace.packages ~fn:(fun (pkg: Package_manifest.t) -> pkg.name)
                = [ package_name "app"; package_name "std" ]
                && Path.equal std_pkg.path expected_std_root
              then
                Ok ()
              else
                Error "expected ensure_workspace to return a build-ready workspace with registry packages"
          | None -> Error "expected ensure_workspace to project std into the workspace")

let test_ensure_lock_repairs_broken_registry_dependency_scopes = fun _ctx ->
  with_tempdir
    "riot_deps_repair_registry_scopes"
    (fun workspace_root ->
      let workspace_manifest = Path.(workspace_root / Path.v "riot.toml") in
      Fs.write "[workspace]\nmembers = [\"packages/app\"]\n" workspace_manifest
      |> Result.expect ~msg:"expected workspace manifest to be written";
      write_file
        Path.(workspace_root / Path.v "packages/app/riot.toml")
        "[package]\nname = \"app\"\n";
      let requirement =
        Std.Version.parse_requirement "*"
        |> Result.expect ~msg:"expected requirement to parse"
      in
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "std" (source ~version:requirement ()) ]
          ()
      in
      let registry_cache =
        Pkgs_ml.Registry_cache.create
          ~riot_home:Path.(workspace_root / Path.v ".riot")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to initialize"
      in
      let registry =
        Pkgs_ml.Registry.in_memory
          ~cache:registry_cache
          ~packages:[
            make_registry_document
              ~name:"std"
              ~latest:"0.2.0"
              ~releases:[ make_release ~version:"0.2.0" () ]
              ();
            make_registry_document
              ~name:"kernel"
              ~latest:"1.0.0"
              ~releases:[ make_release ~version:"1.0.0" () ]
              ();
            make_registry_document
              ~name:"fixme"
              ~latest:"0.5.0"
              ~releases:[ make_release ~version:"0.5.0" () ]
              ();
            make_registry_document
              ~name:"propane"
              ~latest:"0.3.0"
              ~releases:[ make_release ~version:"0.3.0" () ]
              ();
          ]
          ~releases:[
            {
              Pkgs_ml.Registry.package_name = "std";
              version = "0.2.0";
              manifest_toml = {|
[package]
name = "std"
version = "0.2.0"

[dependencies]
kernel = { path = "../kernel", version = "*" }

[build-dependencies]
fixme = { path = "../fixme", version = "*" }

[dev-dependencies]
propane = { path = "../propane", version = "*" }
|};
              files = [];
            };
            {
              Pkgs_ml.Registry.package_name = "kernel";
              version = "1.0.0";
              manifest_toml = {|
[package]
name = "kernel"
version = "1.0.0"
|};
              files = [];
            };
            {
              Pkgs_ml.Registry.package_name = "fixme";
              version = "0.5.0";
              manifest_toml = {|
[package]
name = "fixme"
version = "0.5.0"
|};
              files = [];
            };
            {
              Pkgs_ml.Registry.package_name = "propane";
              version = "0.3.0";
              manifest_toml = {|
[package]
name = "propane"
version = "0.3.0"
|};
              files = [];
            };
          ]
          ()
      in
      let existing_lock =
        Riot_model.Lockfile.{
          format_version = 1;
          dependency_hash =
            Riot_deps.Lock_refresh.dependency_hash
              ~workspace_manager:(workspace_manager ())
              ~workspace_root
              ~manifest_paths:[
                workspace_manifest;
                Path.(workspace_root / Path.v "packages/app/riot.toml");
              ]
            |> Result.expect ~msg:"expected dependency hash to compute";
          packages =
            [
              {
                id =
                  {
                    registry = None;
                    name = package_name "app";
                    version = None;
                    sha256 = None;
                  };
                root = Some (Path.v "packages/app");
                provenance = Workspace;
                dependencies =
                  [
                    {
                      name = package_name "std";
                      package =
                        {
                          registry = Some "pkgs.ml";
                          name = package_name "std";
                          version = Some "0.2.0";
                          sha256 = None;
                        };
                    };
                  ];
                build_dependencies = [];
                dev_dependencies = [];
              };
              {
                id =
                  {
                    registry = Some "pkgs.ml";
                    name = package_name "std";
                    version = Some "0.2.0";
                    sha256 = None;
                  };
                root = None;
                provenance = Registry { registry = "pkgs.ml" };
                dependencies =
                  [
                    {
                      name = package_name "kernel";
                      package =
                        {
                          registry = Some "pkgs.ml";
                          name = package_name "kernel";
                          version = Some "1.0.0";
                          sha256 = None;
                        };
                    };
                  ];
                build_dependencies = [];
                dev_dependencies = [];
              };
              {
                id =
                  {
                    registry = Some "pkgs.ml";
                    name = package_name "kernel";
                    version = Some "1.0.0";
                    sha256 = None;
                  };
                root = None;
                provenance = Registry { registry = "pkgs.ml" };
                dependencies = [];
                build_dependencies = [];
                dev_dependencies = [];
              };
            ];
        }
      in
      Riot_deps.Lockfile_store.write ~workspace_root existing_lock
      |> Result.expect ~msg:"expected initial lockfile write to succeed";
      match Riot_deps.ensure_lock
        ~workspace_manager:(workspace_manager ())
        ~mode:Riot_deps.Dep_solver.Refresh
        ~registry
        ~workspace:(make_workspace_manifest ~workspace_root [ app_pkg ])
        () with
      | Error err ->
          Error ("expected ensure_lock to repair broken registry dependency scopes: "
          ^ pm_error_message err)
      | Ok (lockfile, resolved) ->
          let std_lock =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Riot_model.Lockfile.package) ->
                has_name "std" pkg.id.name && pkg.id.version = Some "0.2.0")
          in
          match std_lock with
          | Some std_lock ->
              if List.length resolved = 5
              && List.length lockfile.packages = 5
              && List.length std_lock.build_dependencies = 1
              && List.length std_lock.dev_dependencies = 1
              && (
                (
                  List.head std_lock.build_dependencies
                  |> Option.expect ~msg:"expected repaired build dependency"
                ).name
                |> has_name "fixme"
              )
              && (
                (
                  List.head std_lock.dev_dependencies
                  |> Option.expect ~msg:"expected repaired dev dependency"
                ).name
                |> has_name "propane"
              ) then
                Ok ()
              else
                Error "expected ensure_lock to rewrite stale registry dependency scopes"
          | None -> Error "expected repaired lockfile to include std")

let test_ensure_workspace_preserves_declared_external_binaries = fun _ctx ->
  with_tempdir
    "riot_deps_ensure_workspace_bins"
    (fun workspace_root ->
      let workspace_manifest = Path.(workspace_root / Path.v "riot.toml") in
      Fs.write "[workspace]\nmembers = [\"packages/app\"]\n" workspace_manifest
      |> Result.expect ~msg:"expected workspace manifest to be written";
      write_file
        Path.(workspace_root / Path.v "packages/app/riot.toml")
        "[package]\nname = \"app\"\n";
      let registry_cache =
        Pkgs_ml.Registry_cache.create
          ~riot_home:Path.(workspace_root / Path.v ".riot")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to initialize"
      in
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "std" (source ~version:Std.Version.any ()) ]
          ()
      in
      let app_pkg =
        Riot_model.Package.make
          ~name:app_pkg.name
          ~path:app_pkg.path
          ~relative_path:(Path.v "packages/app")
          ~dependencies:app_pkg.dependencies
          ~dev_dependencies:app_pkg.dev_dependencies
          ~build_dependencies:app_pkg.build_dependencies
          ~foreign_dependencies:app_pkg.foreign_dependencies
          ~binaries:app_pkg.binaries
          ?library:app_pkg.library
          ~sources:app_pkg.sources
          ~compiler:app_pkg.compiler
          ~commands:app_pkg.commands
          ~fix_providers:app_pkg.fix_providers
          ~publish:app_pkg.publish
          ()
      in
      let workspace =
        Riot_model.Workspace_manifest.make_realized ~root:workspace_root ~packages:[ app_pkg ] ()
      in
      let registry =
        Pkgs_ml.Registry.in_memory
          ~cache:registry_cache
          ~packages:[
            make_registry_document
              ~name:"std"
              ~latest:"0.2.0"
              ~releases:[ make_release ~version:"0.2.0" () ]
              ();
          ]
          ~releases:[
            {
              Pkgs_ml.Registry.package_name = "std";
              version = "0.2.0";
              manifest_toml = {|
[package]
name = "std"
version = "0.2.0"

[[bin]]
name = "std-example"
path = "examples/std_example.ml"

[[bin]]
name = "std-tests"
path = "tests/std_tests.ml"
|};
              files = [];
            };
          ]
          ()
      in
      let workspace_manager = workspace_manager () in
      match Riot_deps.ensure_workspace
        ~workspace_manager
        ~mode:Riot_deps.Dep_solver.Refresh
        ~registry
        ~workspace
        () with
      | Error err -> Error ("expected ensure_workspace to succeed: " ^ pm_error_message err)
      | Ok resolved_workspace ->
          match List.find
            resolved_workspace.packages
            ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> has_name "std" pkg.name) with
          | None -> Error "expected ensure_workspace to project std into the workspace"
          | Some std_pkg ->
              let binary_names =
                std_pkg.declared_binaries
                |> List.map ~fn:(fun (bin: Riot_model.Package.binary) -> bin.name)
                |> List.sort ~compare:String.compare
              in
              if binary_names = [ "std-example"; "std-tests" ] then
                Ok ()
              else
                Error "expected ensure_workspace to preserve declared external binaries across projection")

let test_projection_resolves_workspace_packages = fun _ctx ->
  let std_pkg = make_package ~name:"std" ~path:(Path.v "/workspace/packages/std") () in
  let app_pkg =
    make_package
      ~name:"app"
      ~path:(Path.v "/workspace/packages/app")
      ~dependencies:[ dependency "std" (source ~workspace:true ()) ]
      ()
  in
  let lockfile =
    run_lock_deps ~mode:Riot_deps.Dep_solver.Refresh ~existing_lock:None [ app_pkg; std_pkg ]
    |> Result.expect ~msg:"expected lock projection to succeed"
  in
  match Riot_deps.Projection.resolve_packages
    ~registry:(make_registry [])
    ~workspace_root:(Path.v "/workspace")
    ~packages:(manifests_of_packages [ app_pkg; std_pkg ])
    ~lockfile
    () with
  | Error err ->
      Error ("expected projection to resolve workspace packages: " ^ pm_error_message err)
  | Ok resolved ->
      let app =
        List.head resolved
        |> Option.expect ~msg:"expected resolved app package"
      in
      if List.length resolved = 2
      && has_name "app" app.id.name
      && List.length app.runtime_resolved = 1
      && (
        (
          List.head app.runtime_resolved
          |> Option.expect ~msg:"expected resolved runtime dependency"
        ).resolved_id.name
        |> has_name "std"
      ) then
        Ok ()
      else
        Error "expected projection to preserve resolved runtime dependency ids"

let test_projection_loads_external_manifests_from_lockfile = fun _ctx ->
  with_tempdir
    "riot_deps_projection_external"
    (fun workspace_root ->
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "std" (source ~version:Std.Version.any ()) ]
          ()
      in
      let std_root = Path.(workspace_root / Path.v ".riot/registry/pkgs.ml/src/std/0.2.0") in
      let kernel_root = Path.(workspace_root / Path.v ".riot/registry/pkgs.ml/src/kernel/1.0.0") in
      let fixme_root = Path.(workspace_root / Path.v ".riot/registry/pkgs.ml/src/fixme/0.5.0") in
      let std_manifest_path = Path.(std_root / Path.v "riot.toml") in
      let kernel_manifest_path = Path.(kernel_root / Path.v "riot.toml") in
      let fixme_manifest_path = Path.(fixme_root / Path.v "riot.toml") in
      Fs.create_dir_all std_root
      |> Result.expect ~msg:"expected std root to be created";
      Fs.create_dir_all kernel_root
      |> Result.expect ~msg:"expected kernel root to be created";
      Fs.create_dir_all fixme_root
      |> Result.expect ~msg:"expected fixme root to be created";
      Fs.write
        {|
[package]
name = "std"
version = "0.2.0"

[dependencies]
kernel = "*"

[build-dependencies]
fixme = "*"
|}
        std_manifest_path
      |> Result.expect ~msg:"expected std manifest to be written";
      Fs.write {|
[package]
name = "kernel"
version = "1.0.0"
|} kernel_manifest_path
      |> Result.expect ~msg:"expected kernel manifest to be written";
      Fs.write {|
[package]
name = "fixme"
version = "0.5.0"
|} fixme_manifest_path
      |> Result.expect ~msg:"expected fixme manifest to be written";
      let lockfile =
        Riot_model.Lockfile.{
          format_version = 1;
          dependency_hash = "test";
          packages =
            [
              {
                id =
                  {
                    registry = None;
                    name = package_name "app";
                    version = None;
                    sha256 = None;
                  };
                root = Some (Path.v "packages/app");
                provenance = Workspace;
                dependencies =
                  [
                    {
                      name = package_name "std";
                      package =
                        {
                          registry = Some "pkgs.ml";
                          name = package_name "std";
                          version = Some "0.2.0";
                          sha256 = None;
                        };
                    };
                  ];
                build_dependencies = [];
                dev_dependencies = [];
              };
              {
                id =
                  {
                    registry = Some "pkgs.ml";
                    name = package_name "std";
                    version = Some "0.2.0";
                    sha256 = None;
                  };
                root = None;
                provenance = Registry { registry = "pkgs.ml" };
                dependencies =
                  [
                    {
                      name = package_name "kernel";
                      package =
                        {
                          registry = Some "pkgs.ml";
                          name = package_name "kernel";
                          version = Some "1.0.0";
                          sha256 = None;
                        };
                    };
                  ];
                build_dependencies =
                  [
                    {
                      name = package_name "fixme";
                      package =
                        {
                          registry = Some "pkgs.ml";
                          name = package_name "fixme";
                          version = Some "0.5.0";
                          sha256 = None;
                        };
                    };
                  ];
                dev_dependencies = [];
              };
              {
                id =
                  {
                    registry = Some "pkgs.ml";
                    name = package_name "kernel";
                    version = Some "1.0.0";
                    sha256 = None;
                  };
                root = None;
                provenance = Registry { registry = "pkgs.ml" };
                dependencies = [];
                build_dependencies = [];
                dev_dependencies = [];
              };
              {
                id =
                  {
                    registry = Some "pkgs.ml";
                    name = package_name "fixme";
                    version = Some "0.5.0";
                    sha256 = None;
                  };
                root = None;
                provenance = Registry { registry = "pkgs.ml" };
                dependencies = [];
                build_dependencies = [];
                dev_dependencies = [];
              };
            ];
        }
      in
      let registry_cache =
        Pkgs_ml.Registry_cache.create
          ~riot_home:Path.(workspace_root / Path.v ".riot")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to initialize"
      in
      let registry = Pkgs_ml.Registry.in_memory ~cache:registry_cache ~packages:[] () in
      match collect_event_names
        (fun emit ->
          Riot_deps.Projection.resolve_packages
            ~emit
            ~registry
            ~workspace_root
            ~packages:(manifests_of_packages [ app_pkg ])
            ~lockfile
            ()) with
      | Error err ->
          Error ("expected projection to load external manifests: " ^ pm_error_message err)
      | Ok (resolved, event_names) ->
          let std_resolved =
            List.find
              resolved
              ~fn:(fun (pkg: Riot_model.Package.resolved) ->
                has_name "std" pkg.id.name && pkg.id.version = Some "0.2.0")
          in
          let kernel_resolved =
            List.find
              resolved
              ~fn:(fun (pkg: Riot_model.Package.resolved) ->
                has_name "kernel" pkg.id.name && pkg.id.version = Some "1.0.0")
          in
          let fixme_resolved =
            List.find
              resolved
              ~fn:(fun (pkg: Riot_model.Package.resolved) ->
                has_name "fixme" pkg.id.name && pkg.id.version = Some "0.5.0")
          in
          match (std_resolved, kernel_resolved, fixme_resolved) with
          | (Some std_resolved, Some kernel_resolved, Some fixme_resolved) ->
              if List.length resolved = 4
              && List.contains event_names ~value:"riot.deps.package.manifest.fetch.started"
              && List.contains event_names ~value:"riot.deps.package.manifest.fetch.finished"
              && List.contains event_names ~value:"riot.deps.package.resolved_for_build"
              && Path.to_string std_resolved.materialized_root = Path.to_string std_root
              && List.length std_resolved.runtime_resolved = 1
              && List.length std_resolved.build_resolved = 1
              && (
                (
                  List.head std_resolved.runtime_resolved
                  |> Option.expect ~msg:"expected resolved std runtime dependency"
                ).resolved_id.name
                |> has_name "kernel"
              )
              && (
                (
                  List.head std_resolved.build_resolved
                  |> Option.expect ~msg:"expected resolved std build dependency"
                ).resolved_id.name
                |> has_name "fixme"
              )
              && Path.to_string kernel_resolved.materialized_root = Path.to_string kernel_root
              && Path.to_string fixme_resolved.materialized_root = Path.to_string fixme_root then
                Ok ()
              else
                Error "expected projection to include external lockfile packages"
          | _ ->
              Error "expected projection to resolve std, kernel, and fixme from external manifests")

let test_lock_deps_preserves_registry_build_and_dev_dependencies = fun _ctx ->
  with_tempdir
    "riot_deps_registry_scopes"
    (fun workspace_root ->
      let requirement =
        Std.Version.parse_requirement "*"
        |> Result.expect ~msg:"expected requirement to parse"
      in
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "std" (source ~version:requirement ()) ]
          ()
      in
      let registry_cache =
        Pkgs_ml.Registry_cache.create
          ~riot_home:Path.(workspace_root / Path.v ".riot")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to initialize"
      in
      let registry =
        Pkgs_ml.Registry.in_memory
          ~cache:registry_cache
          ~packages:[
            make_registry_document
              ~name:"std"
              ~latest:"0.2.0"
              ~releases:[ make_release ~version:"0.2.0" () ]
              ();
            make_registry_document
              ~name:"kernel"
              ~latest:"1.0.0"
              ~releases:[ make_release ~version:"1.0.0" () ]
              ();
            make_registry_document
              ~name:"fixme"
              ~latest:"0.5.0"
              ~releases:[ make_release ~version:"0.5.0" () ]
              ();
            make_registry_document
              ~name:"propane"
              ~latest:"0.3.0"
              ~releases:[ make_release ~version:"0.3.0" () ]
              ();
          ]
          ~releases:[
            {
              Pkgs_ml.Registry.package_name = "std";
              version = "0.2.0";
              manifest_toml = {|
[package]
name = "std"
version = "0.2.0"

[dependencies]
kernel = { path = "../kernel", version = "*" }

[build-dependencies]
fixme = { path = "../fixme", version = "*" }

[dev-dependencies]
propane = { path = "../propane", version = "*" }
|};
              files = [];
            };
            {
              Pkgs_ml.Registry.package_name = "kernel";
              version = "1.0.0";
              manifest_toml = {|
[package]
name = "kernel"
version = "1.0.0"
|};
              files = [];
            };
            {
              Pkgs_ml.Registry.package_name = "fixme";
              version = "0.5.0";
              manifest_toml = {|
[package]
name = "fixme"
version = "0.5.0"
|};
              files = [];
            };
            {
              Pkgs_ml.Registry.package_name = "propane";
              version = "0.3.0";
              manifest_toml = {|
[package]
name = "propane"
version = "0.3.0"
|};
              files = [];
            };
          ]
          ()
      in
      match run_lock_deps ~registry ~workspace_root ~mode:Refresh ~existing_lock:None [ app_pkg ] with
      | Error err ->
          Error ("expected lock solve to preserve registry dependency scopes: "
          ^ pm_error_message err)
      | Ok lockfile ->
          let std_lock =
            List.find
              lockfile.packages
              ~fn:(fun (pkg: Riot_model.Lockfile.package) ->
                has_name "std" pkg.id.name && pkg.id.version = Some "0.2.0")
          in
          match std_lock with
          | Some std_lock ->
              if List.length lockfile.packages = 5
              && List.length std_lock.dependencies = 1
              && List.length std_lock.build_dependencies = 1
              && List.length std_lock.dev_dependencies = 1
              && (
                (
                  List.head std_lock.dependencies
                  |> Option.expect ~msg:"expected std runtime dependency"
                ).name
                |> has_name "kernel"
              )
              && (
                (
                  List.head std_lock.build_dependencies
                  |> Option.expect ~msg:"expected std build dependency"
                ).name
                |> has_name "fixme"
              )
              && (
                (
                  List.head std_lock.dev_dependencies
                  |> Option.expect ~msg:"expected std dev dependency"
                ).name
                |> has_name "propane"
              ) then
                Ok ()
              else
                Error "expected registry lock package to preserve runtime, build, and dev dependencies"
          | None -> Error "expected std to appear in the solved lockfile")

let test_projection_bubbles_external_manifest_errors = fun _ctx ->
  with_tempdir
    "riot_deps_projection_manifest_error"
    (fun workspace_root ->
      let app_pkg =
        make_package
          ~name:"app"
          ~path:Path.(workspace_root / Path.v "packages/app")
          ~dependencies:[ dependency "std" (source ~version:Std.Version.any ()) ]
          ()
      in
      let std_root = Path.(workspace_root / Path.v ".riot/registry/pkgs.ml/src/std/0.2.0") in
      let std_manifest_path = Path.(std_root / Path.v "riot.toml") in
      Fs.create_dir_all std_root
      |> Result.expect ~msg:"expected std root to be created";
      Fs.write
        {|
[package]
name = "std"
version = "0.2.0"

[dependencies]
kernel = 123
|}
        std_manifest_path
      |> Result.expect ~msg:"expected invalid std manifest to be written";
      let lockfile =
        Riot_model.Lockfile.{
          format_version = 1;
          dependency_hash = "test";
          packages =
            [
              {
                id =
                  {
                    registry = None;
                    name = package_name "app";
                    version = None;
                    sha256 = None;
                  };
                root = Some (Path.v "packages/app");
                provenance = Workspace;
                dependencies =
                  [
                    {
                      name = package_name "std";
                      package =
                        {
                          registry = Some "pkgs.ml";
                          name = package_name "std";
                          version = Some "0.2.0";
                          sha256 = None;
                        };
                    };
                  ];
                build_dependencies = [];
                dev_dependencies = [];
              };
              {
                id =
                  {
                    registry = Some "pkgs.ml";
                    name = package_name "std";
                    version = Some "0.2.0";
                    sha256 = None;
                  };
                root = None;
                provenance = Registry { registry = "pkgs.ml" };
                dependencies = [];
                build_dependencies = [];
                dev_dependencies = [];
              };
            ];
        }
      in
      let registry_cache =
        Pkgs_ml.Registry_cache.create
          ~riot_home:Path.(workspace_root / Path.v ".riot")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to initialize"
      in
      let registry = Pkgs_ml.Registry.in_memory ~cache:registry_cache ~packages:[] () in
      match Riot_deps.Projection.resolve_packages
        ~registry
        ~workspace_root
        ~packages:(manifests_of_packages [ app_pkg ])
        ~lockfile
        () with
      | Ok _ -> Error "expected invalid external manifest to fail projection"
      | Error err ->
          let message = pm_error_message err in
          if
            String.contains message "must be a string or table"
            || String.contains message "failed to decode package manifest"
          then
            Ok ()
          else
            Error ("unexpected projection error: " ^ pm_error_message err))

let test_projection_fails_when_lockfile_is_missing_package = fun _ctx ->
  let app_pkg = make_package ~name:"app" ~path:(Path.v "/workspace/packages/app") () in
  let lockfile =
    Riot_model.Lockfile.{ format_version = 1; dependency_hash = "test"; packages = [] }
  in
  match Riot_deps.Projection.resolve_packages
    ~registry:(make_registry [])
    ~workspace_root:(Path.v "/workspace")
    ~packages:(manifests_of_packages [ app_pkg ])
    ~lockfile
    () with
  | Ok _ -> Error "expected projection to fail when lockfile is missing package"
  | Error err ->
      if String.contains (pm_error_message err) "lockfile is missing package 'app'" then
        Ok ()
      else
        Error ("unexpected error: " ^ pm_error_message err)

let test_git_dependency_parse_source_locator_accepts_github_shorthand = fun _ctx ->
  match Riot_deps.Git_dependency.parse_source_locator "leostera/riot/packages/riot-cli" with
  | Ok locator ->
      if
        String.equal locator.host "github.com"
        && String.equal locator.owner "leostera"
        && String.equal locator.repo "riot"
        && Option.map locator.subdir ~fn:Path.to_string = Some "packages/riot-cli"
        && Riot_deps.Git_dependency.looks_like_remote_spec "leostera/riot/packages/riot-cli"
      then
        Ok ()
      else
        Error "unexpected shorthand source locator decode"
  | Error err ->
      Error ("expected github shorthand to parse: " ^ Riot_deps.Git_dependency.message err)

let test_load_registry_workspace_materializes_release = fun _ctx ->
  with_tempdir
    "riot_deps_load_registry_workspace"
    (fun root ->
      let registry_cache =
        Pkgs_ml.Registry_cache.create
          ~riot_home:Path.(root / Path.v ".riot")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to initialize"
      in
      let version = "0.1.0" in
      let registry =
        Pkgs_ml.Registry.in_memory
          ~cache:registry_cache
          ~packages:[
            make_registry_document
              ~name:"demo"
              ~latest:version
              ~releases:[ make_release ~version () ]
              ();
          ]
          ~releases:[
            {
              Pkgs_ml.Registry.package_name = "demo";
              version;
              manifest_toml = {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
              files = [ { path = Path.v "src/demo.ml"; contents = "let answer = 42\n" } ];
            };
          ]
          ()
      in
      match Riot_deps.load_registry_workspace
        ~registry
        ~workspace_manager:(workspace_manager ())
        ~spec:"demo"
        () with
      | Ok loaded ->
          if
            has_name "demo" loaded.package_name
            && Int.equal (List.length loaded.workspace.packages) 1
            && Path.equal
              loaded.workspace.root
              (Pkgs_ml.Registry_cache.package_src_dir registry_cache ~package_name:"demo" ~version)
          then
            Ok ()
          else
            Error "unexpected loaded registry workspace"
      | Error err ->
          Error ("expected registry workspace load to succeed: "
          ^ Riot_deps.package_error_message err))

let test_load_registry_workspace_rejects_yanked_release = fun _ctx ->
  with_tempdir
    "riot_deps_load_registry_workspace_yanked"
    (fun root ->
      let registry_cache =
        Pkgs_ml.Registry_cache.create
          ~riot_home:Path.(root / Path.v ".riot")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to initialize"
      in
      let version = "0.1.0" in
      let registry =
        Pkgs_ml.Registry.in_memory
          ~cache:registry_cache
          ~packages:[
            make_registry_document
              ~name:"demo"
              ~latest:version
              ~releases:[ make_release ~version ~yanked:true () ]
              ();
          ]
          ()
      in
      match Riot_deps.load_registry_workspace
        ~registry
        ~workspace_manager:(workspace_manager ())
        ~spec:"demo@0.1.0"
        () with
      | Error (Riot_deps.RegistryReleaseYanked { package; version; registry }) ->
          if
            String.equal package "demo"
            && String.equal version "0.1.0"
            && String.equal registry "pkgs.ml"
          then
            Ok ()
          else
            Error "expected yanked registry release error to preserve package identity"
      | Error err ->
          Error ("expected yanked registry release error, got: "
          ^ Riot_deps.package_error_message err)
      | Ok _ -> Error "expected yanked registry workspace load to fail")

let test_registry_package_spec_roundtrips_bare_name = fun _ctx ->
  match Riot_deps.Registry_package_spec.from_string "demo" with
  | Ok spec ->
      if String.equal (Riot_deps.Registry_package_spec.to_string spec) "demo" then
        Ok ()
      else
        Error "expected bare registry package spec to render without @*"
  | Error err ->
      Error ("expected bare registry package spec to parse: "
      ^ Riot_deps.Registry_package_spec.error_message err)

let test_registry_package_spec_preserves_explicit_requirement = fun _ctx ->
  match Riot_deps.Registry_package_spec.from_string "demo@>= 1.2.3" with
  | Ok spec ->
      if String.equal (Riot_deps.Registry_package_spec.to_string spec) "demo@>= 1.2.3" then
        Ok ()
      else
        Error "expected explicit registry requirement to roundtrip"
  | Error err ->
      Error ("expected explicit registry package spec to parse: "
      ^ Riot_deps.Registry_package_spec.error_message err)

let test_registry_package_spec_reports_invalid_shape = fun _ctx ->
  match Riot_deps.Registry_package_spec.from_string "demo@1.0.0@extra" with
  | Error (Riot_deps.Registry_package_spec.InvalidShape { spec }) ->
      if String.equal spec "demo@1.0.0@extra" then
        Ok ()
      else
        Error "expected invalid shape error to preserve original spec"
  | Error err ->
      Error ("expected invalid shape error, got: "
      ^ Riot_deps.Registry_package_spec.error_message err)
  | Ok _ -> Error "expected invalid registry package spec shape to fail"

let test_registry_package_spec_reports_invalid_package_name = fun _ctx ->
  match Riot_deps.Registry_package_spec.from_string "Demo@1.0.0" with
  | Error (
    Riot_deps.Registry_package_spec.InvalidPackageName {
      spec;
      name;
      error = Riot_model.Package_name.InvalidLeadingCharacter _;
    }
  ) ->
      if String.equal spec "Demo@1.0.0" && String.equal name "Demo" then
        Ok ()
      else
        Error "expected invalid package name error to preserve spec and parsed name"
  | Error err ->
      Error ("expected invalid package name error, got: "
      ^ Riot_deps.Registry_package_spec.error_message err)
  | Ok _ -> Error "expected invalid registry package name to fail"

let test_registry_package_spec_reports_invalid_requirement = fun _ctx ->
  match Riot_deps.Registry_package_spec.from_string "demo@>= nope" with
  | Error (Riot_deps.Registry_package_spec.InvalidRequirement { spec; requirement; _ }) ->
      if String.equal spec "demo@>= nope" && String.equal requirement ">= nope" then
        Ok ()
      else
        Error "expected invalid requirement error to preserve spec and requirement"
  | Error err ->
      Error ("expected invalid requirement error, got: "
      ^ Riot_deps.Registry_package_spec.error_message err)
  | Ok _ -> Error "expected invalid registry requirement to fail"

let tests =
  Test.[
    case
      "dep solver: projects workspace packages into lockfile"
      test_lock_deps_projects_workspace_packages;
    case "dep solver: resolves path dependencies" test_lock_deps_resolves_path_dependencies;
    case
      "dep solver: resolves transitive path dependencies"
      test_lock_deps_resolves_transitive_path_dependencies;
    case
      "dep solver: missing path+version deps fall back to registry resolution"
      test_lock_deps_falls_back_to_registry_when_path_dependency_is_missing;
    case
      "dep solver: collapses workspace path dependencies"
      test_lock_deps_collapses_workspace_path_dependencies;
    case
      "dep solver: resolves registry dependencies to exact versions"
      test_lock_deps_resolves_registry_dependencies_to_exact_versions;
    case
      "dep solver: reports missing registry packages with required-by context"
      test_lock_deps_reports_missing_registry_package_with_required_by;
    case
      "dep solver: prefers workspace packages over registry for matching names"
      test_lock_deps_prefers_workspace_packages_over_registry_for_matching_names;
    case
      "dep solver: prefers available local packages over registry dependencies"
      test_lock_deps_prefers_available_local_packages_over_registry_dependencies;
    case "dep solver: ignores builtin dependencies" test_lock_deps_ignores_builtin_dependencies;
    case
      "dep solver: ignores builtin registry release dependencies"
      test_lock_deps_ignores_builtin_registry_release_dependencies;
    case
      "dep solver: handles cyclic registry dependencies"
      test_lock_deps_handles_cyclic_registry_dependencies;
    case
      "dep solver: handles cyclic local path dependencies"
      test_lock_deps_handles_cyclic_local_path_dependencies;
    case
      "dep solver: refresh preserves existing registry versions"
      test_lock_refresh_preserves_existing_registry_version;
    case
      "dep solver: refresh discards stale external nodes"
      test_lock_refresh_discards_stale_external_nodes;
    case
      "dep solver: refresh discards removed workspace packages"
      test_lock_refresh_discards_removed_workspace_packages;
    case
      "dep solver: unlock discards existing external nodes"
      test_unlock_discards_existing_external_nodes;
    case
      "package management: update targets only requested registry packages"
      test_update_targets_only_requested_registry_packages;
    case "lock refresh: missing lock requires refresh" test_lock_refresh_requires_lock_when_missing;
    case
      "lock refresh: matching dependency hash avoids refresh"
      test_lock_refresh_false_when_dependency_hash_matches;
    case
      "lock refresh: changed dependency hash requires refresh"
      test_lock_refresh_true_when_dependency_hash_changes;
    case
      "lock refresh: reports non-table dependency sections"
      test_lock_refresh_reports_non_table_dependency_sections;
    case "lockfile store: roundtrips root lockfile" test_lockfile_store_roundtrips;
    case
      "lockfile store: missing lockfile returns none"
      test_lockfile_store_returns_none_when_missing;
    case "lockfile store: bubbles parse errors" test_lockfile_store_bubbles_parse_errors;
    case
      "package management: add discovers path dependency package names and refreshes lockfile"
      test_add_path_dependency_discovers_package_name_and_refreshes_lock;
    case
      "package management: add accepts multiple dependencies"
      test_add_multiple_path_dependencies_refreshes_lock_once;
    case
      "package management: add reports missing path dependency manifests"
      test_add_path_dependency_reports_missing_manifest;
    case
      "git dependency: github source spec normalizes into locator and ref"
      test_git_dependency_parse_spec_normalizes_github_source;
    case
      "git dependency: reports multiple ref suffixes"
      test_git_dependency_parse_spec_reports_multiple_ref_suffixes;
    case
      "git dependency: reports invalid source locator shape"
      test_git_dependency_parse_source_locator_reports_invalid_shape;
    case
      "git dependency: github shorthand locator parses for remote commands"
      test_git_dependency_parse_source_locator_accepts_github_shorthand;
    case
      "git dependency: sync checkout clones a local repository"
      test_git_dependency_sync_checkout_clones_local_repo;
    case
      ~size:Large
      "git dependency: sync checkout skips fetch without update"
      test_git_dependency_sync_checkout_skips_fetch_without_update;
    case
      "package management: add rejects unsupported source dependency specs"
      test_add_rejects_unsupported_source_dependency_specs;
    case
      "package management: renders typed source dependency load errors"
      test_package_error_message_renders_typed_source_dependency_errors;
    case
      "package management: renders typed workspace reload errors"
      test_package_error_message_renders_typed_workspace_reload_errors;
    case
      "package management: renders typed registry initialization errors"
      test_package_error_message_renders_typed_registry_initialization_errors;
    case
      ~size:Large
      "package management: renders typed registry operation errors"
      test_package_error_message_renders_typed_registry_operation_errors;
    case
      "package management: add not-found message lists search suggestions"
      test_package_error_message_lists_search_suggestions;
    case "package management: search returns registry results" test_search_returns_registry_results;
    case
      "package management: remove rejects dependencies only inherited from workspace root"
      test_remove_reports_missing_package_dependency_when_only_inherited_from_workspace;
    case
      "package management: remove reports typed manifest update errors"
      test_remove_reports_typed_manifest_update_errors;
    case
      "package management: remove accepts multiple dependencies"
      test_remove_multiple_dependencies_refreshes_lock_once;
    case
      "ensure lock: refreshes missing lock and resolves workspace graph"
      test_ensure_lock_refreshes_missing_lock_and_resolves_workspace;
    case "ensure lock: uses existing fresh lock" test_ensure_lock_uses_existing_fresh_lock;
    case
      "ensure lock: materializes registry packages during projection"
      test_ensure_lock_materializes_registry_packages_during_projection;
    case
      "ensure lock: reuses existing lock and repairs missing registry packages"
      test_ensure_lock_reuses_existing_lock_and_repairs_missing_registry_packages;
    case
      "ensure lock: repairs broken registry dependency scopes"
      test_ensure_lock_repairs_broken_registry_dependency_scopes;
    case
      "ensure workspace: projects materialized registry packages"
      test_ensure_workspace_projects_materialized_registry_packages;
    case
      "ensure workspace: preserves declared external binaries"
      test_ensure_workspace_preserves_declared_external_binaries;
    case
      "package management: load registry workspace materializes release"
      test_load_registry_workspace_materializes_release;
    case
      "package management: load registry workspace rejects yanked release"
      test_load_registry_workspace_rejects_yanked_release;
    case
      "lock deps: preserves registry build and dev dependencies"
      test_lock_deps_preserves_registry_build_and_dev_dependencies;
    case
      "registry package spec: bare names roundtrip without synthetic any markers"
      test_registry_package_spec_roundtrips_bare_name;
    case
      "registry package spec: explicit requirements roundtrip"
      test_registry_package_spec_preserves_explicit_requirement;
    case
      "registry package spec: reports invalid shapes"
      test_registry_package_spec_reports_invalid_shape;
    case
      "registry package spec: reports invalid package names"
      test_registry_package_spec_reports_invalid_package_name;
    case
      "registry package spec: reports invalid requirements"
      test_registry_package_spec_reports_invalid_requirement;
    case
      "projection: resolves workspace packages from lockfile"
      test_projection_resolves_workspace_packages;
    case
      "projection: loads external manifests from lockfile"
      test_projection_loads_external_manifests_from_lockfile;
    case
      "projection: bubbles external manifest errors"
      test_projection_bubbles_external_manifest_errors;
    case
      "projection: fails when lockfile is missing package"
      test_projection_fails_when_lockfile_is_missing_package;
    case
      "publisher: rejects path-only runtime dependencies"
      test_publisher_rejects_path_only_runtime_dependencies;
    case
      "publisher: allows path+version runtime dependencies"
      test_publisher_allows_path_with_version_runtime_dependencies;
    case "publisher: creates package-root tarball" test_publisher_creates_package_root_tarball;
    case "publisher: rejects symlink entries" test_publisher_rejects_symlink_entries;
    case
      "publisher: publishes prepared package artifacts"
      test_publisher_publishes_prepared_artifact;
    case "publisher: bubbles registry publish errors" test_publisher_bubbles_registry_publish_errors;
    case
      "publisher: reports missing prepared artifacts"
      test_publisher_reports_missing_prepared_artifact;
    case
      "publisher: workspace publish order uses runtime local dependencies"
      test_publisher_workspace_publish_order_uses_runtime_local_dependencies;
    case
      "publisher: workspace publish order ignores dev and build dependencies"
      test_publisher_workspace_publish_order_ignores_dev_and_build_dependencies;
    case
      "publisher: workspace publish order reports cycles"
      test_publisher_workspace_publish_order_reports_cycles;
    case
      "publisher: validate registry deps skips workspace publish set"
      test_publisher_validate_registry_dependencies_skips_workspace_publish_set;
    case
      ~size:Large
      "git provenance: discovers nested package locator"
      test_git_provenance_discovers_nested_package_locator;
    case
      ~size:Large
      "git provenance: discovers repo root locator"
      test_git_provenance_discovers_repo_root_locator;
    case
      "git provenance: reports non-git repositories"
      test_git_provenance_reports_non_git_repository;
    case
      "publisher: prepare_publish discovers git provenance automatically"
      test_publisher_prepare_publish_discovers_git_provenance_without_registry;
    case
      "publisher: publish discovers git provenance automatically"
      test_publisher_publish_discovers_git_provenance;
  ]

let name = "Riot PM Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
