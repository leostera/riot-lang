open Std
open Riot_model

module Package_manifest = Package_manifest

let registry_version = "0.0.1"

let bench_config: Bench.bench_config = { iterations = 20; warmup = 3 }

type fixture = {
  workspace: Riot_model.Workspace_manifest.t;
  registry: Pkgs_ml.Registry.t;
  lockfile: Riot_model.Lockfile.t;
  app_package: Riot_model.Package.t;
  fetch_counter: fetch_counter;
}

and fetch_counter = { mutable count: int }

let dependency_source = fun requirement ->
  Package.{
    workspace = false;
    builtin = false;
    path = None;
    source_locator = None;
    ref_ = None;
    version = Some requirement;
  }

let package_name = fun name ->
  Package_name.from_string name
  |> Result.expect ~msg:"expected valid package name"

let make_package = fun ~name ~path ~relative_path ~dependencies ->
  let name = package_name name in
  Package.make
    ~name
    ~path
    ~relative_path
    ~dependencies
    ~publish:{
      version = Some (Std.Version.make ~major:0 ~minor:0 ~patch:1 ());
      description = Some ("Package " ^ Package_name.to_string name);
      license = Some "Apache-2.0";
      is_public = Some true;
    }
    ()

let write_file = fun path contents ->
  let parent =
    match Path.parent path with
    | Some parent -> parent
    | None -> Path.v "."
  in
  Fs.create_dir_all parent
  |> Result.expect ~msg:"expected parent directory to exist";
  Fs.write contents path
  |> Result.expect ~msg:"expected file to be written"

let dependency_line = fun name -> name ^ " = \"*\""

let package_manifest = fun ~name ~dependencies ->
  let header = [ "[package]"; "name = \"" ^ name ^ "\""; "version = \"" ^ registry_version ^ "\"" ]
  in
  let body =
    match dependencies with
    | [] -> []
    | deps -> "" :: "[dependencies]" :: List.map deps ~fn:dependency_line
  in
  String.concat "\n" ((header @ body) @ [ "" ])

let registry_dependency = fun name ->
  Pkgs_ml.Sparse_index.{ name; raw = Data.Json.Object [ ("name", Data.Json.String name); ] }

let make_release = fun ~name ~dependencies ->
  Pkgs_ml.Sparse_index.{
    version = registry_version;
    published_at = "2026-04-02T00:00:00Z";
    canonical_locator = "github.com/example/" ^ name;
    repo_url = "https://github.com/example/" ^ name;
    subdir = ".";
    artifact_sha256 = "sha256-" ^ name;
    description = Some ("Package " ^ name);
    license = Some "Apache-2.0";
    homepage = None;
    repository = Some ("https://github.com/example/" ^ name);
    root_module = None;
    categories = [];
    keywords = [];
    manifest_key = "manifests/" ^ name ^ "/" ^ registry_version ^ ".json";
    source_key = "sources/" ^ name ^ "/" ^ registry_version ^ ".tar.gz";
    dependencies = List.map dependencies ~fn:registry_dependency;
    yanked = false;
    yanked_at = None;
    yanked_by_github_login = None;
  }

let make_document = fun ~name ~dependencies ->
  Pkgs_ml.Sparse_index.{
    schema_version = 1;
    name;
    latest = registry_version;
    updated_at = "2026-04-02T00:00:00Z";
    releases = [ make_release ~name ~dependencies ];
  }

let make_registry_names = fun count ->
  let rec loop acc index =
    if index < 0 then
      acc
    else
      loop (("dep" ^ Int.to_string index) :: acc) (index - 1)
  in
  loop [] (count - 1)

let registry_documents = fun names ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | [ name ] -> loop (make_document ~name ~dependencies:[] :: acc) []
    | name :: ((next_name :: _) as rest) ->
        loop (make_document ~name ~dependencies:[ next_name ] :: acc) rest
  in
  loop [] names

let write_registry_package_roots = fun ~cache ~(lockfile:Riot_model.Lockfile.t) ->
  let write_package_root (pkg: Riot_model.Lockfile.package) =
    match (pkg.provenance, pkg.id.version) with
    | (Riot_model.Lockfile.Registry _, Some version) ->
        let package_root =
          Pkgs_ml.Registry_cache.package_src_dir
            cache
            ~package_name:(Package_name.to_string pkg.id.name)
            ~version
        in
        let manifest =
          package_manifest
            ~name:(Package_name.to_string pkg.id.name)
            ~dependencies:(List.map
              pkg.dependencies
              ~fn:(fun (dep: Riot_model.Lockfile.dependency) -> Package_name.to_string dep.name))
        in
        write_file Path.(package_root / Path.v "riot.toml") manifest
    | _ -> ()
  in
  List.for_each lockfile.packages ~fn:write_package_root

let registry_fetch = fun counter ->
  Pkgs_ml.Registry.make_fetch
    ~get:(fun uri ->
      counter.count <- counter.count + 1;
      Error ("unexpected registry fetch during warm benchmark: " ^ Net.Uri.to_string uri))
    ()

let prepare_fixture = fun root ->
  let workspace_root = Path.(root / Path.v "workspace") in
  let app_root = Path.(workspace_root / Path.v "packages/app") in
  let app_relative = Path.v "packages/app" in
  let registry_names = make_registry_names 40 in
  let root_dependency =
    match registry_names with
    | first :: _ -> first
    | [] -> panic "expected benchmark registry package names"
  in
  write_file
    Path.(workspace_root / Path.v "riot.toml")
    "[workspace]\nmembers = [\"packages/app\"]\n\n[dependencies]\n";
  write_file
    Path.(app_root / Path.v "riot.toml")
    ("[package]\nname = \"app\"\nversion = \"0.0.1\"\n\n[dependencies]\n"
    ^ root_dependency
    ^ " = \"*\"\n");
  let app_package =
    make_package
      ~name:"app"
      ~path:app_root
      ~relative_path:app_relative
      ~dependencies:[
        Package.{
          name = package_name root_dependency;
          source = dependency_source Std.Version.any;
        };
      ]
  in
  let workspace =
    Riot_model.Workspace_manifest.make_realized
      ~root:workspace_root
      ~packages:[ app_package ]
      ~dependencies:[]
      ~dev_dependencies:[]
      ~build_dependencies:[]
      ()
  in
  let riot_home = Path.(workspace_root / Path.v ".riot") in
  let cache =
    Pkgs_ml.Registry_cache.create ~riot_home ~registry_name:"pkgs.ml" ()
    |> Result.expect ~msg:"expected registry cache to initialize"
  in
  let solve_registry =
    Pkgs_ml.Registry.in_memory ~cache ~packages:(registry_documents registry_names) ()
  in
  let lockfile =
    Riot_deps.Dep_solver.lock_deps
      ~mode:Riot_deps.Dep_solver.Refresh
      ~registry:solve_registry
      ~existing_lock:None
      ~workspace
      ()
    |> Result.expect ~msg:"expected lockfile to solve for warm benchmark"
  in
  let workspace_manager = Riot_model.Workspace_manager.create () in
  let dependency_hash =
    Riot_deps.Lock_refresh.dependency_hash
      ~workspace_manager
      ~workspace_root
      ~manifest_paths:[
        Path.(workspace_root / Path.v "riot.toml");
        Path.(app_root / Path.v "riot.toml");
      ]
    |> Result.expect ~msg:"expected dependency hash to compute"
  in
  let lockfile = { lockfile with dependency_hash } in
  Riot_deps.Lockfile_store.write ~workspace_root lockfile
  |> Result.expect ~msg:"expected warm benchmark lockfile to be written";
  write_registry_package_roots ~cache ~lockfile;
  let fetch_counter = { count = 0 } in
  let registry =
    Pkgs_ml.Registry.create_filesystem
      ~fetch:(registry_fetch fetch_counter)
      ~registry_name:"pkgs.ml"
      ~riot_home
      ()
    |> Result.expect ~msg:"expected filesystem registry to initialize"
  in
  {
    workspace;
    registry;
    lockfile;
    app_package;
    fetch_counter;
  }

let assert_no_fetches = fun counter ->
  if Int.equal counter.count 0 then
    ()
  else
    panic ("expected warm path to avoid registry fetches, saw " ^ Int.to_string counter.count)

let bench_materializer_cache_hit = fun (fixture: fixture) () ->
  fixture.fetch_counter.count <- 0;
  Riot_deps.Materializer.ensure_packages ~registry:fixture.registry ~lockfile:fixture.lockfile ()
  |> Result.expect ~msg:"expected materializer cache-hit benchmark to succeed";
  assert_no_fetches fixture.fetch_counter

let bench_projection_warm = fun (fixture: fixture) () ->
  fixture.fetch_counter.count <- 0;
  let _ =
    Riot_deps.Projection.resolve_packages
      ~registry:fixture.registry
      ~workspace_root:fixture.workspace.root
      ~packages:[ Package_manifest.from_package fixture.app_package ]
      ~lockfile:fixture.lockfile
      ()
    |> Result.expect ~msg:"expected warm projection benchmark to succeed"
  in
  assert_no_fetches fixture.fetch_counter

let bench_ensure_lock_warm = fun (fixture: fixture) () ->
  fixture.fetch_counter.count <- 0;
  let workspace_manager = Riot_model.Workspace_manager.create () in
  let _ =
    Riot_deps.ensure_lock
      ~workspace_manager
      ~mode:Riot_deps.Dep_solver.Refresh
      ~registry:fixture.registry
      ~workspace:fixture.workspace
      ()
    |> Result.expect ~msg:"expected warm ensure_lock benchmark to succeed"
  in
  assert_no_fetches fixture.fetch_counter

let benchmark_suite = fun fixture ->
  Bench.[
    with_config
      ~config:bench_config
      "materializer cache hit (40 registry packages)"
      (bench_materializer_cache_hit fixture);
    with_config
      ~config:bench_config
      "projection warm (40 registry packages)"
      (bench_projection_warm fixture);
    with_config
      ~config:bench_config
      "ensure_lock warm (40 registry packages)"
      (bench_ensure_lock_warm fixture);
  ]

let main ~args =
  match Fs.with_tempdir
    ~prefix:"riot_deps_bench"
    (fun root ->
      let fixture = prepare_fixture root in
      Bench.Cli.main ~name:"riot-deps warm path" ~benchmarks:(benchmark_suite fixture) ~args) with
  | Ok result -> result
  | Error err -> panic ("failed to prepare warm lock benchmark: " ^ IO.error_message err)

let () = Runtime.run ~main ~args:Env.args ()
