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

let tests =
  Test.[
    case "registry cache: uses cargo-style split layout" test_registry_split_layout;
    case "sparse index: resolves cache path from normalized package name" test_sparse_index_layout;
    case "sparse index: parses package documents" test_sparse_index_document_parsing;
  ]

let name = "pkgs-ml Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
