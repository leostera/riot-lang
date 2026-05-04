open Std
open Std.Result.Syntax

module Test = Std.Test

let parse_toml = fun source ->
  Std.Data.Toml.parse source
  |> Result.map_err ~fn:Std.Data.Toml.error_to_string

let test_of_toml_accepts_minimal_lockfile = fun _ctx ->
  let source =
    {|
format_version = 1
dependency_hash = "deadbeef"

[[packages]]
name = "demo"
root = "packages/demo"
provenance = { kind = "workspace" }
dependencies = []
build_dependencies = []
dev_dependencies = []
|}
  in
  let* toml = parse_toml source in
  match Riot_model.Lockfile.from_toml toml with
  | Ok lockfile ->
      if
        lockfile.format_version = 1
        && String.equal lockfile.dependency_hash "deadbeef"
        && List.length lockfile.packages = 1
      then
        Ok ()
      else
        Error "expected minimal lockfile to decode"
  | Error err ->
      Error ("expected minimal lockfile to decode, got: " ^ Riot_model.Lockfile.error_message err)

let test_of_toml_reports_missing_dependency_hash = fun _ctx ->
  let source = {|
format_version = 1
packages = []
|}
  in
  let* toml = parse_toml source in
  match Riot_model.Lockfile.from_toml toml with
  | Error (
    Riot_model.Lockfile.MissingField {
      container = Riot_model.Lockfile.Lockfile;
      field = "dependency_hash";
    }
  ) ->
      Ok ()
  | Error err ->
      Error ("expected missing dependency_hash, got: " ^ Riot_model.Lockfile.error_message err)
  | Ok _ -> Error "expected lockfile decode to fail when dependency_hash is missing"

let test_of_toml_reports_invalid_dependency_name = fun _ctx ->
  let source =
    {|
format_version = 1
dependency_hash = "deadbeef"

[[packages]]
name = "demo"
root = "packages/demo"
provenance = { kind = "workspace" }
dependencies = [{ name = "Std", version = "1.0.0", sha256 = "abc" }]
build_dependencies = []
dev_dependencies = []
|}
  in
  let* toml = parse_toml source in
  match Riot_model.Lockfile.from_toml toml with
  | Error (
    Riot_model.Lockfile.InvalidPackageName {
      container = Riot_model.Lockfile.Dependency;
      field = "name";
      value = "Std";
      _;
    }
  ) ->
      Ok ()
  | Error err ->
      Error ("expected invalid dependency package name, got: "
      ^ Riot_model.Lockfile.error_message err)
  | Ok _ -> Error "expected lockfile decode to fail for invalid dependency package name"

let test_of_toml_reports_unknown_provenance_kind = fun _ctx ->
  let source =
    {|
format_version = 1
dependency_hash = "deadbeef"

[[packages]]
name = "demo"
root = "packages/demo"
provenance = { kind = "banana" }
dependencies = []
build_dependencies = []
dev_dependencies = []
|}
  in
  let* toml = parse_toml source in
  match Riot_model.Lockfile.from_toml toml with
  | Error (Riot_model.Lockfile.UnknownProvenanceKind { value = "banana" }) -> Ok ()
  | Error err ->
      Error ("expected unknown provenance kind, got: " ^ Riot_model.Lockfile.error_message err)
  | Ok _ -> Error "expected lockfile decode to fail for unknown provenance kind"

let tests =
  Test.[
    case "Lockfile.from_toml accepts minimal lockfiles" test_of_toml_accepts_minimal_lockfile;
    case
      "Lockfile.from_toml reports missing dependency_hash"
      test_of_toml_reports_missing_dependency_hash;
    case
      "Lockfile.from_toml reports invalid dependency names"
      test_of_toml_reports_invalid_dependency_name;
    case
      "Lockfile.from_toml reports unknown provenance kinds"
      test_of_toml_reports_unknown_provenance_kind;
  ]

let main ~args = Test.Cli.main ~name:"lockfile" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
