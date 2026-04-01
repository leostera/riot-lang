open Std
module Test = Std.Test

let test_registry_split_layout = fun () ->
  let cache =
    Pkgs_ml.Registry_cache.create
      ~tusk_home:(Path.v "/tmp/.tusk")
      ~registry_name:"pkgs.ml"
    |> Result.expect ~msg:"expected registry cache to be created"
  in
  let index = Pkgs_ml.Registry_cache.index_dir cache |> Path.to_string in
  let archive =
    Pkgs_ml.Registry_cache.archive_path cache ~package_name:"std" ~version:"0.1.0"
    |> Path.to_string
  in
  let src =
    Pkgs_ml.Registry_cache.package_src_dir cache ~package_name:"std" ~version:"0.1.0"
    |> Path.to_string
  in
  if
    String.equal index "/tmp/.tusk/registry/pkgs.ml/index"
    && String.equal archive "/tmp/.tusk/registry/pkgs.ml/archive/std/0.1.0.tar"
    && String.equal src "/tmp/.tusk/registry/pkgs.ml/src/std/0.1.0"
  then
    Ok ()
  else
    Error
      ("unexpected registry layout:\nindex="
      ^ index
      ^ "\narchive="
      ^ archive
      ^ "\nsrc="
      ^ src)

let test_sparse_index_layout = fun () ->
  let cache =
    Pkgs_ml.Registry_cache.create
      ~tusk_home:(Path.v "/tmp/.tusk")
      ~registry_name:"pkgs.ml"
    |> Result.expect ~msg:"expected registry cache to be created"
  in
  let actual =
    Pkgs_ml.Sparse_index.package_cache_path cache ~package_name:"AbCd"
    |> Path.to_string
  in
  if String.equal actual "/tmp/.tusk/registry/pkgs.ml/index/ab/cd/abcd.json" then
    Ok ()
  else
    Error ("unexpected sparse index cache path: " ^ actual)

let test_sparse_index_document_parsing = fun () ->
  let source =
    {|{
  "schema_version": 1,
  "name": "kernel",
  "latest": "0.0.1",
  "updated_at": "2026-03-27T15:27:35Z",
  "releases": [
    {
      "version": "0.0.1",
      "published_at": "2026-03-27T15:27:35Z",
      "canonical_locator": "github.com/leostera/riot-new/packages/kernel",
      "repo_url": "https://github.com/leostera/riot-new",
      "subdir": "packages/kernel",
      "sha": "2aef0372bf5b6687db05bda80cde55f960cbfd9d",
      "manifest_key": "packages/github.com/leostera/riot-new/packages/kernel/2aef0372bf5b6687db05bda80cde55f960cbfd9d.manifest.json",
      "source_key": "sources/github.com/leostera/riot-new/2aef0372bf5b6687db05bda80cde55f960cbfd9d.tar.gz",
      "dependencies": [{ "name": "std", "path": "../std" }]
    }
  ]
}|}
  in
  match Pkgs_ml.Sparse_index.package_document_of_string source with
  | Ok document ->
      if
        String.equal document.name "kernel"
        && String.equal document.latest "0.0.1"
        && List.length document.releases = 1
      then
        Ok ()
      else
        Error "unexpected sparse index document contents"
  | Error err -> Error err

let test_sparse_index_cached_reads = fun () ->
  let config =
    Pkgs_ml.Sparse_index.config_of_string
      {|{
  "schema_version": 1,
  "kind": "sparse",
  "package_path_strategy": "cargo-lowercase-v1",
  "index_base_url": "https://cdn.pkgs.ml/index/v1",
  "artifact_base_url": "https://cdn.pkgs.ml"
}|}
    |> Result.expect ~msg:"expected sparse index config to parse"
  in
  let package =
    Pkgs_ml.Sparse_index.package_document_of_string
      {|{
  "schema_version": 1,
  "name": "kernel",
  "latest": "0.0.1",
  "updated_at": "2026-03-27T15:27:35Z",
  "releases": []
}|}
    |> Result.expect ~msg:"expected sparse index package to parse"
  in
  let registry =
    Pkgs_ml.Registry.in_memory
      ~config
      ~packages:[ package ]
      ()
  in
  match
    Pkgs_ml.Registry.read_config registry,
    Pkgs_ml.Registry.read_package_document registry ~package_name:"Kernel"
  with
  | Ok (Some actual_config), Ok (Some actual_package)
    when
      String.equal actual_config.kind "sparse"
      && String.equal actual_package.name "kernel" -> Ok ()
  | Ok _, Ok _ -> Error "expected in-memory registry to return config and normalized package lookup"
  | Error err, _
  | _, Error err -> Error err

let test_registry_materializes_in_memory_release = fun () ->
  match
    Fs.with_tempdir ~prefix:"pkgs_ml_materialize"
      (fun tempdir ->
        let cache =
          Pkgs_ml.Registry_cache.create
            ~tusk_home:Path.(tempdir / Path.v ".tusk")
            ~registry_name:"pkgs.ml"
          |> Result.expect ~msg:"expected registry cache to be created"
        in
        let package =
          Pkgs_ml.Sparse_index.{
            schema_version = 1;
            name = "std";
            latest = "0.1.0";
            updated_at = "2026-04-01T00:00:00Z";
            releases = [];
          }
        in
        let registry =
          Pkgs_ml.Registry.in_memory
            ~packages:[ package ]
            ~releases:[
              {
                package_name = "std";
                version = "0.1.0";
                manifest_toml = "[package]\nname = \"std\"\n";
                files = [
                  { path = Path.v "src/std.ml"; contents = "let answer = 42\n" };
                  { path = Path.v "README.md"; contents = "# std\n" };
                ];
              };
            ]
            ()
        in
        match Pkgs_ml.Registry.materialize_release registry ~cache ~package_name:"std" ~version:"0.1.0" with
        | Error err -> Error err
        | Ok `Already_present -> Error "expected in-memory release to be materialized on first attempt"
        | Ok `Materialized ->
            let manifest_path =
              Pkgs_ml.Registry_cache.package_src_dir cache ~package_name:"std" ~version:"0.1.0"
              |> fun root -> Path.(root / Path.v "tusk.toml")
            in
            let source_path =
              Pkgs_ml.Registry_cache.package_src_dir cache ~package_name:"std" ~version:"0.1.0"
              |> fun root -> Path.(root / Path.v "src/std.ml")
            in
            match Fs.read manifest_path, Fs.read source_path with
            | Ok manifest, Ok source
              when
                String.equal manifest "[package]\nname = \"std\"\n"
                && String.equal source "let answer = 42\n" -> Ok ()
            | Ok _, Ok _ -> Error "expected materialized release contents to roundtrip from the in-memory registry"
            | Error err, _
            | _, Error err -> Error (IO.error_message err))
  with
  | Error err -> Error (IO.error_message err)
  | Ok result -> result

let test_registry_materialize_skips_existing_release = fun () ->
  match
    Fs.with_tempdir ~prefix:"pkgs_ml_materialize_skip"
      (fun tempdir ->
        let cache =
          Pkgs_ml.Registry_cache.create
            ~tusk_home:Path.(tempdir / Path.v ".tusk")
            ~registry_name:"pkgs.ml"
          |> Result.expect ~msg:"expected registry cache to be created"
        in
        let registry =
          Pkgs_ml.Registry.in_memory
            ~packages:[]
            ~releases:[
              {
                package_name = "std";
                version = "0.1.0";
                manifest_toml = "[package]\nname = \"std\"\n";
                files = [];
              };
            ]
            ()
        in
        match Pkgs_ml.Registry.materialize_release registry ~cache ~package_name:"std" ~version:"0.1.0" with
        | Error err -> Error err
        | Ok _ -> (
            match Pkgs_ml.Registry.materialize_release registry ~cache ~package_name:"std" ~version:"0.1.0" with
            | Ok `Already_present -> Ok ()
            | Ok `Materialized -> Error "expected second materialization to detect existing package sources"
            | Error err -> Error err
          ))
  with
  | Error err -> Error (IO.error_message err)
  | Ok result -> result

let create_test_archive = fun ~source_root ~archive_path ->
  let archive_parent =
    match Path.parent archive_path with
    | Some parent -> parent
    | None -> Path.v "."
  in
  match Fs.create_dir_all archive_parent with
  | Error err ->
      Error ("failed to create archive parent directory: " ^ IO.error_message err)
  | Ok () -> (
      let cmd = Command.make
        ~args:[ "-cf"; Path.to_string archive_path; "-C"; Path.to_string source_root; "." ]
        "tar"
      in
      match Command.output cmd with
      | Error (Command.SystemError msg) ->
          Error ("failed to create test archive: " ^ msg)
      | Ok output when output.Command.status != 0 ->
          let detail =
            if String.equal output.stderr "" then
              output.stdout
            else
              output.stderr
          in
          Error ("failed to create test archive: " ^ detail)
      | Ok _ -> Ok ()
    )

let test_filesystem_registry_materializes_cached_release = fun () ->
  match
    Fs.with_tempdir ~prefix:"pkgs_ml_filesystem_materialize"
      (fun tempdir ->
        let cache =
          Pkgs_ml.Registry_cache.create
            ~tusk_home:Path.(tempdir / Path.v ".tusk")
            ~registry_name:"pkgs.ml"
          |> Result.expect ~msg:"expected registry cache to be created"
        in
        let source_root = Path.(tempdir / Path.v "source/std-0.1.0") in
        let source_file = Path.(source_root / Path.v "src/std.ml") in
        Fs.create_dir_all Path.(source_root / Path.v "src")
        |> Result.expect ~msg:"expected source directory to be created";
        Fs.write "[package]\nname = \"std\"\nversion = \"0.1.0\"\n" Path.(source_root / Path.v "tusk.toml")
        |> Result.expect ~msg:"expected manifest to be written";
        Fs.write "let answer = 42\n" source_file
        |> Result.expect ~msg:"expected source file to be written";
        let archive_path =
          Pkgs_ml.Registry_cache.archive_path cache ~package_name:"std" ~version:"0.1.0"
        in
        match create_test_archive ~source_root ~archive_path with
        | Error err -> Error err
        | Ok () ->
            let registry = Pkgs_ml.Registry.filesystem cache in
            match Pkgs_ml.Registry.materialize_release registry ~cache ~package_name:"std" ~version:"0.1.0" with
            | Error err -> Error err
            | Ok `Already_present -> Error "expected cached archive to materialize on first attempt"
            | Ok `Materialized ->
                let manifest_path =
                  Pkgs_ml.Registry_cache.package_src_dir cache ~package_name:"std" ~version:"0.1.0"
                  |> fun root -> Path.(root / Path.v "tusk.toml")
                in
                let materialized_source =
                  Pkgs_ml.Registry_cache.package_src_dir cache ~package_name:"std" ~version:"0.1.0"
                  |> fun root -> Path.(root / Path.v "src/std.ml")
                in
                match Fs.read manifest_path, Fs.read materialized_source with
                | Ok manifest, Ok source
                  when
                    String.equal manifest "[package]\nname = \"std\"\nversion = \"0.1.0\"\n"
                    && String.equal source "let answer = 42\n" -> Ok ()
                | Ok _, Ok _ -> Error "expected filesystem registry to extract the cached archive into src/"
                | Error err, _
                | _, Error err -> Error (IO.error_message err))
  with
  | Error err -> Error (IO.error_message err)
  | Ok result -> result

let test_filesystem_registry_errors_for_uncached_release = fun () ->
  match
    Fs.with_tempdir ~prefix:"pkgs_ml_filesystem_registry"
      (fun tempdir ->
        let cache =
          Pkgs_ml.Registry_cache.create
            ~tusk_home:Path.(tempdir / Path.v ".tusk")
            ~registry_name:"pkgs.ml"
          |> Result.expect ~msg:"expected registry cache to be created"
        in
        let registry = Pkgs_ml.Registry.filesystem cache in
        match Pkgs_ml.Registry.materialize_release registry ~cache ~package_name:"std" ~version:"0.1.0" with
        | Ok _ -> Error "expected filesystem registry to fail for uncached releases"
        | Error err ->
            if String.contains err "cannot materialize uncached package" then
              Ok ()
            else
              Error ("unexpected error: " ^ err))
  with
  | Error err -> Error (IO.error_message err)
  | Ok result -> result

let tests =
  Test.[
    case "registry cache: uses cargo-style split layout" test_registry_split_layout;
    case "sparse index: resolves cache path from normalized package name" test_sparse_index_layout;
    case "sparse index: parses package documents" test_sparse_index_document_parsing;
    case "registry: in-memory registry returns config and packages" test_sparse_index_cached_reads;
    case "registry: in-memory registry materializes release source trees" test_registry_materializes_in_memory_release;
    case "registry: materialization skips existing release sources" test_registry_materialize_skips_existing_release;
    case "registry: filesystem registry materializes cached release archives" test_filesystem_registry_materializes_cached_release;
    case "registry: filesystem registry errors for uncached releases" test_filesystem_registry_errors_for_uncached_release;
  ]

let name = "pkgs-ml Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
