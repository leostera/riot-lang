open Std

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let write_toolchain_toml = fun ~root content ->
  Fs.write content Path.(root / Path.v "ocaml-toolchain.toml") |> Result.expect ~msg:"failed to write ocaml-toolchain.toml"

let make_workspace = fun root -> Riot_model.Workspace.make_realized ~root ~packages:[] ()

let target_strings = fun targets -> List.map targets ~fn:System.TargetTriple.to_string

let test_from_workspace_defaults_without_toolchain_file = fun _ctx ->
  with_tempdir "riot_model_toolchain_default"
    (fun root ->
      let config = Riot_model.Toolchain_config.from_root ~root in
      if not (String.equal config.version Riot_model.Toolchain_config.default_ocaml_version) then
        Error "expected default toolchain version when ocaml-toolchain.toml is absent"
      else if not (List.is_empty config.targets) then
        Error "expected no configured targets in the default toolchain config"
      else
        Ok ())

let test_from_workspace_parses_and_normalizes_targets = fun _ctx ->
  with_tempdir "riot_model_toolchain_targets"
    (fun root ->
      let () = write_toolchain_toml ~root "[toolchain]\nversion = \"5.5.0-riot.4\"\ntargets = [\"x86_64-unknown-linux-gnu\", \"bad\", \"aarch64-unknown-linux-gnu\", \"x86_64-unknown-linux-gnu\"]\n" in
      let config = Riot_model.Toolchain_config.from_root ~root in
      if not (String.equal config.version "5.5.0-riot.4") then
        Error "expected toolchain version to parse from ocaml-toolchain.toml"
      else if
        not
          (String.equal (String.concat "," (target_strings config.targets)) "aarch64-unknown-linux-gnu,x86_64-unknown-linux-gnu")
      then
        Error "expected toolchain targets to parse as typed triples, ignore invalid values, and deduplicate duplicates"
      else
        Ok ())

let test_from_workspace_parses_local_path_sources = fun _ctx ->
  with_tempdir "riot_model_toolchain_path"
    (fun root ->
      let () = write_toolchain_toml ~root "[toolchain]\nversion = { path = \"./vendor/ocaml\" }\ntargets = []\n" in
      let config = Riot_model.Toolchain_config.from_root ~root in
      match config.source with
      | Riot_model.Toolchain_config.Path path ->
          if Path.equal path (Path.v "./vendor/ocaml") then
            Ok ()
          else
            Error "expected local path toolchain source to roundtrip"
      | Riot_model.Toolchain_config.Version _
      | Riot_model.Toolchain_config.Url _ -> Error "expected path-based toolchain source to parse as Path")

let tests =
  Test.[
    case "Toolchain_config.from_root defaults without ocaml-toolchain.toml" test_from_workspace_defaults_without_toolchain_file;
    case "Toolchain_config.from_root parses and normalizes typed targets" test_from_workspace_parses_and_normalizes_targets;
    case "Toolchain_config.from_root parses local path sources" test_from_workspace_parses_local_path_sources;
  ]

let main ~args = Test.Cli.main ~name:"toolchain_config" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
