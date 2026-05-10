open Std
open Std.Bench

let scratch_counter = Sync.Atomic.make 0

let temp_root = fun () ->
  match Env.get Env.String ~var:"TMPDIR" with
  | Some dir when dir != "" -> Path.v dir
  | _ ->
      match Env.get Env.String ~var:"TEMP" with
      | Some dir when dir != "" -> Path.v dir
      | _ ->
          match Env.get Env.String ~var:"TMP" with
          | Some dir when dir != "" -> Path.v dir
          | _ -> Path.v "/tmp"

let make_scratch_dir = fun prefix ->
  let pid =
    Process.id ()
    |> Int32.to_string
  in
  let nanos =
    Time.SystemTime.duration_since_epoch ()
    |> Time.Duration.to_nanos
    |> Int64.to_string
  in
  let counter =
    Sync.Atomic.fetch_and_add scratch_counter 1
    |> Int.to_string
  in
  let path =
    Path.(temp_root ()
    / Path.v "pkgs-ml-bench"
    / Path.v (prefix ^ "-" ^ pid ^ "-" ^ nanos ^ "-" ^ counter))
  in
  let _ =
    Fs.create_dir_all path
    |> Result.expect ~msg:"create bench scratch dir should succeed"
  in
  path

let package_document_source =
  {|{
  "schema_version": 1,
  "name": "std",
  "latest": "0.1.0",
  "updated_at": "2026-03-27T15:27:35Z",
  "releases": [
    {
      "version": "0.1.0",
      "published_at": "2026-03-27T15:27:35Z",
      "canonical_locator": "github.com/leostera/riot/packages/std",
      "repo_url": "https://github.com/leostera/riot",
      "subdir": "packages/std",
      "artifact_sha256": "deadbeef",
      "description": "Standard library for Riot",
      "manifest_key": "packages/std/0.1.0/deadbeef.manifest.json",
      "source_key": "sources/std/0.1.0/deadbeef.tar.gz",
      "dependencies": []
    }
  ]
}|}

let make_package_document = fun ~name ~latest ~description ->
  {
    Pkgs_ml.Sparse_index.schema_version = 1;
    name;
    latest;
    updated_at = "2026-03-27T15:27:35Z";
    releases =
      [
        {
          Pkgs_ml.Sparse_index.version = latest;
          published_at = "2026-03-27T15:27:35Z";
          canonical_locator = "github.com/leostera/riot/packages/" ^ name;
          repo_url = "https://github.com/leostera/riot";
          subdir = "packages/" ^ name;
          artifact_sha256 = "deadbeef";
          description = Some description;
          license = None;
          homepage = None;
          repository = None;
          root_module = None;
          categories = [];
          keywords = [];
          manifest_key = "packages/" ^ name ^ "/" ^ latest ^ "/deadbeef.manifest.json";
          source_key = "sources/" ^ name ^ "/" ^ latest ^ "/deadbeef.tar.gz";
          dependencies = [];
          yanked = false;
          yanked_at = None;
          yanked_by_github_login = None;
        };
      ];
  }

let bench_package_relpath = fun () ->
  let _ = Pkgs_ml.Sparse_index.package_relpath "riot-planner" in
  ()

let bench_package_document_parse = fun () ->
  let _ = Pkgs_ml.Sparse_index.package_document_of_string package_document_source in
  ()

let make_bench_search_packages = fun () ->
  let cache =
    Pkgs_ml.Registry_cache.create
      ~riot_home:(make_scratch_dir "search-cache")
      ~registry_name:"pkgs.ml"
      ()
    |> Result.expect ~msg:"create registry cache should succeed"
  in
  let packages =
    List.init
      ~count:256
      ~fn:(fun index ->
        let name =
          if Int.rem index 8 = 0 then
            "riot-" ^ Int.to_string index
          else
            "pkg-" ^ Int.to_string index
        in
        make_package_document ~name ~latest:"0.1.0" ~description:("package " ^ name))
  in
  let registry = Pkgs_ml.Registry.in_memory ~cache ~packages () in
  fun () ->
    let _ =
      Pkgs_ml.Registry.search_packages registry ~query:"riot" ~limit:32 ()
      |> Result.expect ~msg:"search_packages should succeed"
    in
    ()

let make_release_source = fun version ->
  {
    Pkgs_ml.Registry.package_name = "std";
    version;
    manifest_toml = "[package]\nname = \"std\"\nversion = \"" ^ version ^ "\"\n";
    files = [
      { Pkgs_ml.Registry.path = Path.v "src/std.ml"; contents = "let answer = 42\n" };
      { Pkgs_ml.Registry.path = Path.v "README.md"; contents = "# std\n" };
    ];
  }

let make_bench_materialize_release_miss = fun () ->
  let cache =
    Pkgs_ml.Registry_cache.create
      ~riot_home:(make_scratch_dir "materialize-miss-cache")
      ~registry_name:"pkgs.ml"
      ()
    |> Result.expect ~msg:"create registry cache should succeed"
  in
  let counter = ref 0 in
  fun () ->
    let version = "0.1." ^ Int.to_string !counter in
    counter := !counter + 1;
    let registry =
      Pkgs_ml.Registry.in_memory ~cache ~packages:[] ~releases:[ make_release_source version ] ()
    in
    let _ =
      Pkgs_ml.Registry.materialize_release registry ~package_name:"std" ~version
      |> Result.expect ~msg:"materialize_release miss should succeed"
    in
    ()

let make_bench_materialize_release_hit = fun () ->
  let cache =
    Pkgs_ml.Registry_cache.create
      ~riot_home:(make_scratch_dir "materialize-hit-cache")
      ~registry_name:"pkgs.ml"
      ()
    |> Result.expect ~msg:"create registry cache should succeed"
  in
  let version = "1.0.0" in
  let registry =
    Pkgs_ml.Registry.in_memory ~cache ~packages:[] ~releases:[ make_release_source version ] ()
  in
  let _ =
    Pkgs_ml.Registry.materialize_release registry ~package_name:"std" ~version
    |> Result.expect ~msg:"initial materialize_release should succeed"
  in
  fun () ->
    let _ =
      Pkgs_ml.Registry.materialize_release registry ~package_name:"std" ~version
      |> Result.expect ~msg:"materialize_release hit should succeed"
    in
    ()

let small = { iterations = 500; warmup = 50 }

let medium = { iterations = 150; warmup = 20 }

let heavy = { iterations = 40; warmup = 5 }

let benchmarks =
  Bench.[
    with_config ~config:small "pkgs-ml sparse-index package_relpath" bench_package_relpath;
    with_config
      ~config:medium
      "pkgs-ml sparse-index package_document_of_string"
      bench_package_document_parse;
    with_config
      ~config:medium
      "pkgs-ml registry search_packages in-memory"
      (make_bench_search_packages ());
    with_config
      ~config:heavy
      "pkgs-ml registry materialize_release miss"
      (make_bench_materialize_release_miss ());
    with_config
      ~config:medium
      "pkgs-ml registry materialize_release hit"
      (make_bench_materialize_release_hit ());
  ]

let main ~args = Bench.Cli.main ~name:"pkgs-ml benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
