open Std

module Test = Std.Test

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let with_tempdir_result = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let make_registry_cache = fun riot_home ->
  Pkgs_ml.Registry_cache.create ~riot_home ~registry_name:"pkgs.ml" ()
  |> Result.expect ~msg:"expected registry cache"

let sample_release = fun ~version ->
  Pkgs_ml.Sparse_index.{
    version;
    published_at = "2026-04-21T00:00:00Z";
    canonical_locator = "github.com/leostera/serde-json";
    repo_url = "https://github.com/leostera/serde-json";
    subdir = "";
    artifact_sha256 = "deadbeef";
    description = Some "serde json";
    license = Some "Apache-2.0";
    homepage = Some "https://serde-json.dev";
    repository = Some "https://github.com/leostera/serde-json";
    root_module = Some "Serde_json";
    categories = [ "serialization" ];
    keywords = [ "json" ];
    manifest_key = "packages/serde-json/" ^ version ^ "/deadbeef.manifest.json";
    source_key = "sources/serde-json/" ^ version ^ "/deadbeef.tar.gz";
    dependencies = [];
    yanked = false;
    yanked_at = None;
    yanked_by_github_login = None;
  }

let sample_document = fun () ->
  Pkgs_ml.Sparse_index.{
    schema_version = 1;
    name = "serde-json";
    latest = "1.2.0";
    updated_at = "2026-04-21T00:00:00Z";
    releases = [ sample_release ~version:"1.0.0"; sample_release ~version:"1.2.0" ];
  }

let sample_registry = fun ~riot_home ->
  let cache = make_registry_cache riot_home in
  Pkgs_ml.Registry.in_memory
    ~cache
    ~packages:[ sample_document () ]
    ~releases:[
      {
        Pkgs_ml.Registry.package_name = "serde-json";
        version = "1.2.0";
        manifest_toml = "[package]\nname = \"serde-json\"\nversion = \"1.2.0\"\ndescription = \"serde json\"\nlicense = \"Apache-2.0\"\n";
        files = [ { path = Path.v "src/serde_json.ml"; contents = "let decode = fun _ -> ()\n" } ];
      };
    ]
    ()

let make_local_workspace = fun ?is_public root ->
  let package_root = Path.(root / Path.v "demo") in
  let src_dir = Path.(package_root / Path.v "src") in
  let manifest_path = Path.(package_root / Path.v "riot.toml") in
  let public_field =
    match is_public with
    | None -> ""
    | Some true -> "public = true\n"
    | Some false -> "public = false\n"
  in
  let manifest_toml =
    "[package]\nname = \"demo\"\nversion = \"0.1.0\"\ndescription = \"demo package\"\nlicense = \"Apache-2.0\"\n"
    ^ public_field
    ^ "\n[lib]\npath = \"src/demo.ml\"\n"
  in
  Fs.create_dir_all src_dir
  |> Result.expect ~msg:"expected src dir";
  Fs.write manifest_toml manifest_path
  |> Result.expect ~msg:"expected manifest write";
  Fs.write "let value = 42\n" Path.(src_dir / Path.v "demo.ml")
  |> Result.expect ~msg:"expected source write";
  let manifest =
    Data.Toml.parse manifest_toml
    |> Result.expect ~msg:"expected toml parse"
    |> Riot_model.Package_manifest.from_toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:package_root
      ~relative_path:(Path.v "demo")
    |> Result.expect ~msg:"expected package manifest"
  in
  Riot_model.Workspace_manifest.make ~root ~packages:[ manifest ] ()

let test_info_package_prefers_local_workspace_package = fun _ctx ->
  with_tempdir_result
    "riot_cli_info_local"
    (fun tempdir ->
      let riot_home = Path.(tempdir / Path.v ".riot") in
      let registry = sample_registry ~riot_home in
      let workspace = make_local_workspace tempdir in
      match Riot_cli.Info_package.resolve
        ~registry
        ~local_workspace:(Some (workspace, []))
        ~target:"demo"
        () with
      | Error err -> Error err.message
      | Ok info ->
          Test.assert_equal ~expected:Riot_cli.Info_package.Workspace ~actual:info.source_kind;
          Test.assert_equal ~expected:(Some "0.1.0") ~actual:info.resolved_version;
          Test.assert_equal ~expected:(Some "demo") ~actual:info.relative_path;
          Test.assert_equal ~expected:(Some tempdir) ~actual:info.workspace_root;
          Test.assert_equal ~expected:(Some "demo/riot.toml") ~actual:info.package_path;
          Test.assert_equal ~expected:(Some false) ~actual:info.is_public;
          Test.assert_equal ~expected:None ~actual:info.registry_root;
          Test.assert_equal ~expected:None ~actual:info.registry_name;
          Test.assert_equal ~expected:None ~actual:info.registry_package_path;
          if Option.is_some info.links.docs_url || Option.is_some info.links.package_url then
            Error "expected local package to omit registry docs/package links"
          else
            Ok ())

let test_info_package_private_local_package_omits_registry_links = fun _ctx ->
  with_tempdir_result
    "riot_cli_info_private_local"
    (fun tempdir ->
      let riot_home = Path.(tempdir / Path.v ".riot") in
      let registry = sample_registry ~riot_home in
      let workspace = make_local_workspace ~is_public:false tempdir in
      match Riot_cli.Info_package.resolve
        ~registry
        ~local_workspace:(Some (workspace, []))
        ~target:"demo"
        () with
      | Error err -> Error err.message
      | Ok info ->
          if Option.is_some info.links.docs_url || Option.is_some info.links.package_url then
            Error "expected private local package to omit registry docs/package links"
          else
            Ok ())

let test_info_package_loads_registry_release_and_paths = fun _ctx ->
  with_tempdir_result
    "riot_cli_info_registry"
    (fun tempdir ->
      let riot_home = Path.(tempdir / Path.v ".riot") in
      let registry = sample_registry ~riot_home in
      match Riot_cli.Info_package.resolve
        ~registry
        ~local_workspace:None
        ~target:"serde-json@1.2.0"
        () with
      | Error err -> Error err.message
      | Ok info ->
          Test.assert_equal ~expected:Riot_cli.Info_package.Registry ~actual:info.source_kind;
          Test.assert_equal ~expected:(Some "1.2.0") ~actual:info.resolved_version;
          match Fs.exists info.root with
          | Error err -> Error (IO.error_message err)
          | Ok false -> Error "expected registry package root to be materialized"
          | Ok true ->
              if not (Option.is_some info.registry_package_path) then
                Error "expected registry package path"
              else if not (Option.is_some info.links.docs_url) then
                Error "expected docs url for registry package"
              else
                Ok ())

let test_info_package_json_includes_registry_paths_and_links = fun _ctx ->
  with_tempdir_result
    "riot_cli_info_json"
    (fun tempdir ->
      let riot_home = Path.(tempdir / Path.v ".riot") in
      let registry = sample_registry ~riot_home in
      match Riot_cli.Info_package.resolve ~registry ~local_workspace:None ~target:"serde-json" () with
      | Error err -> Error err.message
      | Ok info ->
          let json = Riot_cli.Info_package.to_json info in
          let registry_root =
            match Data.Json.get_field "registry" json with
            | Some (Data.Json.Object fields) -> Data.Json.get_field "root" (Data.Json.Object fields)
            | _ -> None
          in
          let docs_link =
            match Data.Json.get_field "links" json with
            | Some (Data.Json.Object fields) ->
                Data.Json.get_field "docs_url" (Data.Json.Object fields)
            | _ -> None
          in
          match (registry_root, docs_link) with
          | (Some (Data.Json.String _), Some (Data.Json.String _)) -> Ok ()
          | _ -> Error "expected registry root and docs url in package json")

let test_info_package_json_omits_registry_for_workspace_package = fun _ctx ->
  with_tempdir_result
    "riot_cli_info_json_local"
    (fun tempdir ->
      let riot_home = Path.(tempdir / Path.v ".riot") in
      let registry = sample_registry ~riot_home in
      let workspace = make_local_workspace tempdir in
      match Riot_cli.Info_package.resolve
        ~registry
        ~local_workspace:(Some (workspace, []))
        ~target:"demo"
        () with
      | Error err -> Error err.message
      | Ok info ->
          let json = Riot_cli.Info_package.to_json info in
          let registry_json = Data.Json.get_field "registry" json in
          let workspace_root = Data.Json.get_field "workspace_root" json in
          let package_path = Data.Json.get_field "package_path" json in
          let public = Data.Json.get_field "public" json in
          match (registry_json, workspace_root, package_path, public) with
          | (
            Some Data.Json.Null,
            Some (Data.Json.String _),
            Some (Data.Json.String "demo/riot.toml"),
            Some (Data.Json.Bool false)
          ) -> Ok ()
          | _ ->
              Error "expected workspace package json to omit registry paths and include workspace path metadata")

let test_info_workspace_scan_error_message_renders_typed_errors = fun _ctx ->
  let cwd_message =
    Riot_cli.Info_cmd.workspace_scan_error_message
      (Riot_cli.Info_cmd.CurrentDirReadFailed (Path.SystemError "cwd unavailable"))
  in
  let scan_message =
    Riot_cli.Info_cmd.workspace_scan_error_message
      (Riot_cli.Info_cmd.WorkspaceScanFailed Riot_model.Workspace_manager.NoWorkspaceRootFound)
  in
  let expected_cwd = "failed to read current directory: cwd unavailable" in
  let expected_scan = "no workspace root found" in
  if not (String.equal cwd_message expected_cwd) then
    Error ("unexpected current-dir scan message: " ^ cwd_message)
  else if not (String.equal scan_message expected_scan) then
    Error ("unexpected workspace scan message: " ^ scan_message)
  else
    Ok ()

let tests =
  Test.[
    case
      "info package: bare local package prefers workspace metadata"
      test_info_package_prefers_local_workspace_package;
    case
      "info package: private local package omits registry links"
      test_info_package_private_local_package_omits_registry_links;
    case
      "info package: registry target materializes release and paths"
      test_info_package_loads_registry_release_and_paths;
    case
      "info package: json includes registry paths and links"
      test_info_package_json_includes_registry_paths_and_links;
    case
      "info package: workspace json omits registry paths"
      test_info_package_json_omits_registry_for_workspace_package;
    case
      "info workspace: renders typed scan errors"
      test_info_workspace_scan_error_message_renders_typed_errors;
  ]

let name = "Riot CLI Info Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
