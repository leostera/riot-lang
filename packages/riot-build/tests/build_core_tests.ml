open Std
module Test = Std.Test

let target = fun value ->
  Riot_model.Target.from_string value
  |> Result.expect ~msg:("invalid target triple: " ^ value)

let target_set_of_strings = fun values ->
  values |> List.map ~fn:target |> Riot_model.Target.Set.of_list

let target_strings = fun targets ->
  Riot_model.Target.Set.to_list targets
  |> List.map ~fn:Riot_model.Target.to_string

let target_request_of_cli_options = fun ~all_targets ~target ->
  if all_targets then
    Riot_model.Target.All
  else
    match target with
    | Some value -> Riot_model.Target.parse value
    | None -> Riot_model.Target.Host

let write_workspace_manifest = fun ~root ~members ->
  let members =
    members
    |> List.map ~fn:(fun member -> "  \"" ^ Path.to_string member ^ "\"")
    |> String.concat ",\n"
  in
  let content = "[workspace]\nmembers = [\n" ^ members ^ "\n]\n" in
  Fs.write content Path.(root / Path.v "riot.toml")
  |> Result.expect ~msg:"Write workspace riot.toml failed"

let write_toolchain_config = fun ~root ~targets ->
  let target_lines =
    targets
    |> List.map ~fn:(fun target -> "\"" ^ target ^ "\"")
    |> String.concat ", "
  in
  Fs.write
    ("[toolchain]\nversion = \"" ^ Riot_model.Toolchain_config.default_ocaml_version ^ "\"\ntargets = [" ^ target_lines ^ "]\n")
    Path.(root / Path.v "ocaml-toolchain.toml")
  |> Result.expect ~msg:"Write ocaml-toolchain.toml failed"

let make_package = fun ~root ~name ~value ->
  let pkg_dir = Path.(root / Path.v name) in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"Create src failed" in
  let ml_file = Path.(src_dir / Path.v "lib.ml") in
  let _ = Fs.write ("let value = " ^ value ^ "\n") ml_file |> Result.expect ~msg:"Write ml failed" in
  let riot_file = Path.(pkg_dir / Path.v "riot.toml") in
  let riot_content =
    "[package]\nname = \"" ^ name ^ "\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n"
  in
  let _ = Fs.write riot_content riot_file |> Result.expect ~msg:"Write riot.toml failed" in
  Riot_model.Package.make
    ~name
    ~path:pkg_dir
    ~relative_path:(Path.v name)
    ~library:{ path = Path.v "src/lib.ml" }
    ~sources:{
      src = [ Path.v "src/lib.ml" ];
      native = [];
      tests = [];
      examples = [];
      bench = [];
    }
    ()

let map_with_index = fun items ~fn ->
  let rec loop index acc = function
    | [] -> acc
    | item :: rest ->
        loop (index + 1) (acc @ [ fn index item ]) rest
  in
  loop 0 [] items

let make_valid_workspace = fun ?(package_names = [ "demo" ]) ?toolchain_targets tmpdir ->
  let packages =
    map_with_index package_names ~fn:(fun index name ->
        make_package ~root:tmpdir ~name ~value:(Int.to_string (index + 1)))
  in
  let members = List.map package_names ~fn:Path.v in
  let _ = write_workspace_manifest ~root:tmpdir ~members in
  let () =
    match toolchain_targets with
    | Some targets -> write_toolchain_config ~root:tmpdir ~targets
    | None -> ()
  in
  Riot_model.Workspace.make_realized ~root:tmpdir ~packages ()

let make_prepared_workspace = fun ?workspace_manager workspace ->
  Riot_build.Prepared_workspace.of_workspace ?workspace_manager workspace

let make_artifact = fun name ->
  {
    Riot_store.Artifact.hash = Crypto.hash_string name;
    files = [ Path.v (name ^ ".cmx") ];
    ocamlc_warnings = [];
    exports = [];
  }

let make_build_result = fun ~(package: Riot_model.Package.t) ~status ->
  {
    Riot_executor.Package_builder.package_key =
      Riot_model.Package.key_of_string (package.name ^ ":runtime");
    package;
    status;
    ocamlc_warnings = [];
    duration = Time.Duration.zero;
  }

let make_request = fun
  ?(packages = [ "demo" ])
  ?(targets = Riot_model.Target.Host)
  ?(scope = Riot_build.Request.Runtime)
  ?(profile = Riot_model.Profile.debug)
  ()
  ->
  Riot_build.Request.make ~packages ~targets ~scope ~profile ()

let resolve_request = fun prepared_workspace request ->
  Riot_build.Build_core.resolve prepared_workspace request

let build_request = fun ?on_event prepared_workspace request ->
  let spec =
    resolve_request prepared_workspace request
    |> Result.expect ~msg:"expected build request resolution to succeed"
  in
  Riot_build.Build_core.build ?on_event spec

let build_error_message = Riot_build.Build_core.build_error_message

let phase_name = function
  | Riot_build.Event.TargetsResolved _ -> "targets_resolved"
  | Riot_build.Event.ToolchainsEnsured _ -> "toolchains_ensured"
  | Riot_build.Event.ToolchainsValidated _ -> "toolchains_validated"
  | Riot_build.Event.ClientConnecting -> "client_connecting"
  | Riot_build.Event.ClientConnected -> "client_connected"
  | Riot_build.Event.TargetBuildStarted _ -> "target_build_started"
  | Riot_build.Event.TargetBuildFinished _ -> "target_build_finished"
  | Riot_build.Event.CacheGenerationRecordingStarted _ -> "cache_generation_recording_started"
  | Riot_build.Event.CacheGenerationRecorded _ -> "cache_generation_recorded"
  | Riot_build.Event.ReturningResults _ -> "returning_results"

let expect_subsequence = fun ~haystack ~needle ->
  let rec loop haystack needle =
    match haystack, needle with
    | _, [] -> Ok ()
    | [], _ ->
        Error ("expected phase subsequence " ^ String.concat " -> " needle
        ^ " in "
        ^ String.concat " -> " haystack)
    | actual :: rest_haystack, expected :: rest_needle ->
        if String.equal actual expected then
          loop rest_haystack rest_needle
        else
          loop rest_haystack needle
  in
  loop haystack needle

let test_prepared_workspace_exposes_member_packages = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_prepared_workspace"
      (fun tmpdir ->
        let workspace = make_valid_workspace ~package_names:[ "demo"; "util" ] tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        Test.assert_equal
          ~expected:[ "demo"; "util" ]
          ~actual:(Riot_build.Prepared_workspace.package_names prepared_workspace);
        Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_prepared_workspace_preserves_workspace_manager = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_workspace_manager"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let workspace_manager = Riot_model.Workspace_manager.create () in
        let prepared_workspace =
          make_prepared_workspace ~workspace_manager workspace
        in
        Test.assert_true
          (Option.is_some
             (Riot_build.Prepared_workspace.workspace_manager prepared_workspace));
        Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_prepared_workspace_defaults_workspace_manager_to_none = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_workspace_manager_none"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        Test.assert_true
          (Option.is_none
             (Riot_build.Prepared_workspace.workspace_manager prepared_workspace));
        Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_prepared_workspace_preserves_workspace_package_order = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_workspace_order"
      (fun tmpdir ->
        let workspace =
          make_valid_workspace ~package_names:[ "util"; "demo"; "extra" ] tmpdir
        in
        let prepared_workspace = make_prepared_workspace workspace in
        Test.assert_equal
          ~expected:[ "util"; "demo"; "extra" ]
          ~actual:(Riot_build.Prepared_workspace.package_names prepared_workspace);
        Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_request_preserves_inputs = fun _ctx ->
  let request =
    make_request
      ~packages:[ "demo"; "util" ]
      ~targets:(Riot_model.Target.parse "linux")
      ~scope:Riot_build.Request.Dev
      ~profile:Riot_model.Profile.release
      ()
  in
  Test.assert_equal ~expected:[ "demo"; "util" ] ~actual:(Riot_build.Request.packages request);
  Test.assert_equal
    ~expected:Riot_build.Request.Dev
    ~actual:(Riot_build.Request.scope request);
  Test.assert_equal
    ~expected:"release"
    ~actual:((Riot_build.Request.profile request).name);
  Ok ()

let test_request_preserves_host_target_selector = fun _ctx ->
  match
    Riot_build.Request.targets
      (make_request ~targets:Riot_model.Target.Host ())
  with
  | Riot_model.Target.Host -> Ok ()
  | Riot_model.Target.All
  | Riot_model.Target.Pattern _
  | Riot_model.Target.Exact _ ->
      Error "expected request to preserve host target selector"

let test_target_selector_from_string_normalizes_aliases = fun _ctx ->
  match
    Riot_model.Target.parse "HOST",
    Riot_model.Target.parse "NaTiVe",
    Riot_model.Target.parse "ALL",
    Riot_model.Target.parse "LiNuX"
  with
  | Riot_model.Target.Host,
    Riot_model.Target.Host,
    Riot_model.Target.All,
    Riot_model.Target.Pattern "linux" ->
      Ok ()
  | _ ->
      Error "expected target selector aliases and patterns to normalize case"

let test_target_selector_of_cli_options_prefers_all_targets = fun _ctx ->
  match
    target_request_of_cli_options ~all_targets:true ~target:(Some "linux"),
    target_request_of_cli_options ~all_targets:false ~target:None
  with
  | Riot_model.Target.All, Riot_model.Target.Host ->
      Ok ()
  | _ ->
      Error "expected CLI selector parsing to prefer --all-targets and default to host"

let test_build_spec_preserves_inputs = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_build_spec"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        let spec =
          Riot_build.Build_spec.make
            ~workspace:prepared_workspace
            ~package_names:[ "demo" ]
            ~targets:(target_set_of_strings [ Riot_model.Target.to_string (Riot_model.Riot_dirs.host_target ()) ])
            ~scope:Riot_build.Build_spec.Dev
            ~profile:Riot_model.Profile.release
        in
        Test.assert_equal
          ~expected:[ "demo" ]
          ~actual:(Riot_build.Build_spec.package_names spec);
        Test.assert_equal
          ~expected:[ Riot_model.Target.to_string (Riot_model.Riot_dirs.host_target ()) ]
          ~actual:(target_strings (Riot_build.Build_spec.targets spec));
        Test.assert_equal
          ~expected:Riot_build.Build_spec.Dev
          ~actual:(Riot_build.Build_spec.scope spec);
        Test.assert_equal
          ~expected:"release"
          ~actual:((Riot_build.Build_spec.profile spec).name);
        Test.assert_equal
          ~expected:tmpdir
          ~actual:(
            Riot_build.Build_spec.workspace spec
            |> Riot_build.Prepared_workspace.workspace
            |> fun workspace -> workspace.root
          );
        Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_target_selector_resolve_exact_targets = fun _ctx ->
  let host = target "aarch64-apple-darwin" in
  let configured_targets =
    target_set_of_strings
      [ "aarch64-apple-darwin"; "x86_64-unknown-linux-gnu"; "wasm32-unknown-wasi" ]
  in
  match
    Riot_model.Target.resolve
      ~host
      ~configured_targets
      (Riot_model.Target.Pattern "x86_64-unknown-linux-gnu")
  with
  | Ok targets when target_strings targets = [ "x86_64-unknown-linux-gnu" ] ->
      Ok ()
  | Ok targets ->
      Error ("expected exact target match, got: " ^ String.concat ", " (target_strings targets))
  | Error err ->
      Error ("expected exact target match, got error: " ^ err.pattern)

let test_target_selector_resolve_substring_patterns = fun _ctx ->
  let host = target "aarch64-apple-darwin" in
  let configured_targets =
    target_set_of_strings
      [
        "aarch64-apple-darwin";
        "x86_64-unknown-linux-gnu";
        "aarch64-unknown-linux-gnu";
      ]
  in
  match
    Riot_model.Target.resolve
      ~host
      ~configured_targets
      (Riot_model.Target.Pattern "linux")
  with
  | Ok targets when target_strings targets = [ "aarch64-unknown-linux-gnu"; "x86_64-unknown-linux-gnu" ] ->
      Ok ()
  | Ok targets ->
      Error ("expected substring target matches, got: " ^ String.concat ", " (target_strings targets))
  | Error err ->
      Error ("expected substring target matches, got error: " ^ err.pattern)

let test_target_selector_configured_targets_default_to_host_and_preserve_config = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_target_selector_configured"
      (fun tmpdir ->
        let host = Riot_model.Target.current in
        let default_workspace = make_valid_workspace tmpdir in
        let default_targets =
          Riot_model.Target.configured_targets
            ~host
            (Riot_model.Toolchain_config.from_workspace default_workspace)
        in
        Test.assert_equal ~expected:[ Riot_model.Target.to_string host ] ~actual:(target_strings default_targets);
        let configured_workspace =
          make_valid_workspace
            ~package_names:[ "configured" ]
            ~toolchain_targets:[ "aarch64-unknown-linux-gnu"; "x86_64-unknown-linux-gnu" ]
            tmpdir
        in
        let configured_targets =
          Riot_model.Target.configured_targets
            ~host
            (Riot_model.Toolchain_config.from_workspace configured_workspace)
        in
        Test.assert_equal
          ~expected:[ "aarch64-unknown-linux-gnu"; "x86_64-unknown-linux-gnu" ]
          ~actual:(target_strings configured_targets);
        Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_resolve_returns_a_typed_build_spec = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_resolve"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        let request = make_request ~targets:(Riot_model.Target.parse "host") () in
        match resolve_request prepared_workspace request with
        | Error err ->
            Error ("expected request resolution to succeed, got: " ^ Riot_build.Build_core.resolve_error_message err)
        | Ok spec ->
            Test.assert_equal
              ~expected:[ "demo" ]
              ~actual:(Riot_build.Build_spec.package_names spec);
            Test.assert_equal
              ~expected:[ Riot_model.Target.to_string (Riot_model.Riot_dirs.host_target ()) ]
              ~actual:(target_strings (Riot_build.Build_spec.targets spec));
            Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_top_level_resolve_matches_build_core = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_top_level_resolve"
      (fun tmpdir ->
        let workspace =
          make_valid_workspace
            ~package_names:[ "util"; "demo" ]
            ~toolchain_targets:[ "aarch64-apple-darwin"; "wasm32-unknown-wasi" ]
            tmpdir
        in
        let prepared_workspace = make_prepared_workspace workspace in
        let request =
          make_request
            ~packages:[ "demo" ]
            ~targets:(Riot_model.Target.parse "all")
            ~scope:Riot_build.Request.Dev
            ()
        in
        match
          Riot_build.resolve prepared_workspace request,
          Riot_build.Build_core.resolve prepared_workspace request
        with
        | Ok left, Ok right ->
            Test.assert_equal
              ~expected:(Riot_build.Build_spec.package_names right)
              ~actual:(Riot_build.Build_spec.package_names left);
            Test.assert_equal
              ~expected:(target_strings (Riot_build.Build_spec.targets right))
              ~actual:(target_strings (Riot_build.Build_spec.targets left));
              Test.assert_equal
              ~expected:(Riot_build.Build_spec.scope right)
              ~actual:(Riot_build.Build_spec.scope left);
            Test.assert_equal
              ~expected:((Riot_build.Build_spec.profile right).name)
              ~actual:((Riot_build.Build_spec.profile left).name);
            Ok ()
        | Error err, Ok _ ->
            Error ("expected top-level resolve to match build core resolve, got error: "
            ^ Riot_build.resolve_error_message err)
        | Ok _, Error err ->
            Error ("expected build core resolve to match top-level resolve, got error: "
            ^ Riot_build.Build_core.resolve_error_message err)
        | Error left, Error right ->
            Test.assert_equal
              ~expected:(Riot_build.Build_core.resolve_error_message right)
              ~actual:(Riot_build.resolve_error_message left);
            Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_resolve_preserves_scope_and_profile = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_scope_profile"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        let request =
          make_request
            ~scope:Riot_build.Request.Dev
            ~profile:Riot_model.Profile.release
            ()
        in
        match resolve_request prepared_workspace request with
        | Error err ->
            Error ("expected request resolution to succeed, got: " ^ Riot_build.Build_core.resolve_error_message err)
        | Ok spec ->
            Test.assert_equal
              ~expected:Riot_build.Build_spec.Dev
              ~actual:(Riot_build.Build_spec.scope spec);
            Test.assert_equal
              ~expected:"release"
              ~actual:((Riot_build.Build_spec.profile spec).name);
            Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_resolve_uses_all_packages_when_none_are_requested = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_resolve_all_packages"
      (fun tmpdir ->
        let workspace = make_valid_workspace ~package_names:[ "demo"; "util" ] tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        let request = make_request ~packages:[] () in
        match resolve_request prepared_workspace request with
        | Error err ->
            Error ("expected request resolution to succeed, got: " ^ Riot_build.Build_core.resolve_error_message err)
        | Ok spec ->
            Test.assert_equal
              ~expected:[ "demo"; "util" ]
              ~actual:(Riot_build.Build_spec.package_names spec);
            Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_resolve_all_packages_sorts_available_package_names = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_all_packages_sorted"
      (fun tmpdir ->
        let workspace =
          make_valid_workspace ~package_names:[ "util"; "demo"; "extra" ] tmpdir
        in
        let prepared_workspace = make_prepared_workspace workspace in
        let request = make_request ~packages:[] () in
        match resolve_request prepared_workspace request with
        | Error err ->
            Error ("expected request resolution to succeed, got: " ^ Riot_build.Build_core.resolve_error_message err)
        | Ok spec ->
            Test.assert_equal
              ~expected:[ "demo"; "extra"; "util" ]
              ~actual:(Riot_build.Build_spec.package_names spec);
            Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_resolve_uses_all_configured_targets = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_resolve_all_targets"
      (fun tmpdir ->
        let workspace =
          make_valid_workspace
            ~toolchain_targets:[ "aarch64-apple-darwin"; "aarch64-unknown-linux-gnu" ]
            tmpdir
        in
        let prepared_workspace = make_prepared_workspace workspace in
        let request = make_request ~targets:Riot_model.Target.All () in
        match resolve_request prepared_workspace request with
        | Error err ->
            Error ("expected request resolution to succeed, got: " ^ Riot_build.Build_core.resolve_error_message err)
        | Ok spec ->
            Test.assert_equal
              ~expected:[ "aarch64-apple-darwin"; "aarch64-unknown-linux-gnu" ]
              ~actual:(target_strings (Riot_build.Build_spec.targets spec));
            Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_resolve_matches_configured_target_patterns = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_resolve_target_pattern"
      (fun tmpdir ->
        let workspace =
          make_valid_workspace
            ~toolchain_targets:[ "aarch64-apple-darwin"; "x86_64-unknown-linux-gnu"; "aarch64-unknown-linux-gnu" ]
            tmpdir
        in
        let prepared_workspace = make_prepared_workspace workspace in
        let request =
          make_request ~targets:(Riot_model.Target.parse "linux") ()
        in
        match resolve_request prepared_workspace request with
        | Error err ->
            Error ("expected request resolution to succeed, got: " ^ Riot_build.Build_core.resolve_error_message err)
        | Ok spec ->
            Test.assert_equal
              ~expected:[ "aarch64-unknown-linux-gnu"; "x86_64-unknown-linux-gnu" ]
              ~actual:(target_strings (Riot_build.Build_spec.targets spec));
            Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_resolve_preserves_requested_package_order = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_package_order"
      (fun tmpdir ->
        let workspace =
          make_valid_workspace ~package_names:[ "demo"; "util"; "extra" ] tmpdir
        in
        let prepared_workspace = make_prepared_workspace workspace in
        let request = make_request ~packages:[ "util"; "demo" ] () in
        match resolve_request prepared_workspace request with
        | Error err ->
            Error ("expected request resolution to succeed, got: " ^ Riot_build.Build_core.resolve_error_message err)
        | Ok spec ->
            Test.assert_equal
              ~expected:[ "util"; "demo" ]
              ~actual:(Riot_build.Build_spec.package_names spec);
            Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_resolve_native_alias_uses_host_target = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_native_target"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        let request =
          make_request ~targets:(Riot_model.Target.parse "native") ()
        in
        match resolve_request prepared_workspace request with
        | Error err ->
            Error ("expected request resolution to succeed, got: " ^ Riot_build.Build_core.resolve_error_message err)
        | Ok spec ->
            Test.assert_equal
              ~expected:[ Riot_model.Target.to_string (Riot_model.Riot_dirs.host_target ()) ]
              ~actual:(target_strings (Riot_build.Build_spec.targets spec));
            Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_resolve_all_without_explicit_targets_uses_host = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_all_defaults_host"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        let request = make_request ~targets:Riot_model.Target.All () in
        match resolve_request prepared_workspace request with
        | Error err ->
            Error ("expected request resolution to succeed, got: " ^ Riot_build.Build_core.resolve_error_message err)
        | Ok spec ->
            Test.assert_equal
              ~expected:[ Riot_model.Target.to_string (Riot_model.Riot_dirs.host_target ()) ]
              ~actual:(target_strings (Riot_build.Build_spec.targets spec));
            Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_resolve_reports_missing_package_before_build = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_missing_package"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        let request = make_request ~packages:[ "missing" ] () in
        match resolve_request prepared_workspace request with
        | Error (Riot_build.Build_core.PackageNotFound { package_name; available_packages }) ->
            Test.assert_equal ~expected:"missing" ~actual:package_name;
            Test.assert_equal ~expected:[ "demo" ] ~actual:available_packages;
            Ok ()
        | Error err ->
            Error ("expected package-not-found error, got: " ^ Riot_build.Build_core.resolve_error_message err)
        | Ok _ ->
            Error "expected resolution to fail before build execution")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_resolve_reports_multiple_missing_packages_before_build = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_missing_packages"
      (fun tmpdir ->
        let workspace = make_valid_workspace ~package_names:[ "demo"; "util" ] tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        let request =
          make_request ~packages:[ "demo"; "missing-a"; "missing-b" ] ()
        in
        match resolve_request prepared_workspace request with
        | Error (Riot_build.Build_core.PackagesNotFound { package_names; available_packages }) ->
            Test.assert_equal ~expected:[ "missing-a"; "missing-b" ] ~actual:package_names;
            Test.assert_equal ~expected:[ "demo"; "util" ] ~actual:available_packages;
            Ok ()
        | Error err ->
            Error ("expected packages-not-found error, got: " ^ Riot_build.Build_core.resolve_error_message err)
        | Ok _ ->
            Error "expected resolution to fail before build execution")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_resolve_prefers_missing_packages_over_target_misses = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_resolve_precedence"
      (fun tmpdir ->
        let workspace =
          make_valid_workspace
            ~toolchain_targets:[ "aarch64-apple-darwin" ]
            tmpdir
        in
        let prepared_workspace = make_prepared_workspace workspace in
        let request =
          make_request
            ~packages:[ "missing" ]
            ~targets:(Riot_model.Target.parse "linux")
            ()
        in
        match resolve_request prepared_workspace request with
        | Error (Riot_build.Build_core.PackageNotFound { package_name; _ }) when String.equal package_name "missing" ->
            Ok ()
        | Error err ->
            Error ("expected package-not-found precedence, got: " ^ Riot_build.Build_core.resolve_error_message err)
        | Ok _ ->
            Error "expected resolution to fail before target resolution mattered")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_resolve_error_message_lists_missing_packages = fun _ctx ->
  let message =
    Riot_build.Build_core.resolve_error_message
      (Riot_build.Build_core.PackagesNotFound {
         package_names = [ "missing-a"; "missing-b" ];
         available_packages = [ "demo"; "util" ];
       })
  in
  Test.assert_true (String.contains message "missing-a");
  Test.assert_true (String.contains message "missing-b");
  Test.assert_true (String.contains message "demo");
  Test.assert_true (String.contains message "util");
  Ok ()

let test_resolve_error_message_lists_target_pattern_and_available_targets = fun _ctx ->
  let message =
    Riot_build.Build_core.resolve_error_message
      (Riot_build.Build_core.TargetSelectionFailed {
         pattern = "linux";
         available_targets = [ target "aarch64-apple-darwin"; target "x86_64-apple-darwin" ];
       })
  in
  Test.assert_true (String.contains message "linux");
  Test.assert_true (String.contains message "aarch64-apple-darwin");
  Test.assert_true (String.contains message "x86_64-apple-darwin");
  Ok ()

let test_output_maps_executor_statuses = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_output_mapping"
      (fun tmpdir ->
        let built_pkg = make_package ~root:tmpdir ~name:"built" ~value:"1" in
        let cached_pkg = make_package ~root:tmpdir ~name:"cached" ~value:"2" in
        let skipped_pkg = make_package ~root:tmpdir ~name:"skipped" ~value:"3" in
        let failed_pkg = make_package ~root:tmpdir ~name:"failed" ~value:"4" in
        let output =
          Riot_build.Output.of_build_results
            [
              make_build_result
                ~package:built_pkg
                ~status:(Riot_executor.Package_builder.Built (make_artifact "built"));
              make_build_result
                ~package:cached_pkg
                ~status:(Riot_executor.Package_builder.Cached (make_artifact "cached"));
              make_build_result
                ~package:skipped_pkg
                ~status:(Riot_executor.Package_builder.Skipped { reason = "needs std" });
              make_build_result
                ~package:failed_pkg
                ~status:(Riot_executor.Package_builder.Failed
                  (Riot_executor.Package_builder.ExecutionFailed { message = "boom" }));
            ]
        in
        let expect_status = fun package_name ->
          match Riot_build.Output.find_package output package_name with
          | Some package_output -> Ok package_output
          | None -> Error ("missing output for package " ^ package_name)
        in
        let open Std.Result.Syntax in
        let* built_output = expect_status "built" in
        let* cached_output = expect_status "cached" in
        let* skipped_output = expect_status "skipped" in
        let* failed_output = expect_status "failed" in
        let* () =
          match Riot_build.Output.package_status built_output with
          | Riot_build.Output.Built _ -> Ok ()
          | Riot_build.Output.Cached _
          | Riot_build.Output.Skipped _
          | Riot_build.Output.Failed _ ->
              Error "expected built package output"
        in
        let* () =
          match Riot_build.Output.package_status cached_output with
          | Riot_build.Output.Cached _ -> Ok ()
          | Riot_build.Output.Built _
          | Riot_build.Output.Skipped _
          | Riot_build.Output.Failed _ ->
              Error "expected cached package output"
        in
        let* () =
          match Riot_build.Output.package_status skipped_output with
          | Riot_build.Output.Skipped reason when String.equal reason "needs std" ->
              Ok ()
          | Riot_build.Output.Skipped _ ->
              Error "expected skipped package reason to be preserved"
          | Riot_build.Output.Built _
          | Riot_build.Output.Cached _
          | Riot_build.Output.Failed _ ->
              Error "expected skipped package output"
        in
        match Riot_build.Output.package_status failed_output with
        | Riot_build.Output.Failed message when String.contains message "boom" ->
            Ok ()
        | Riot_build.Output.Failed _ ->
            Error "expected failed package message to preserve execution failure"
        | Riot_build.Output.Built _
        | Riot_build.Output.Cached _
        | Riot_build.Output.Skipped _ ->
            Error "expected failed package output"
      )
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_output_preserves_build_result_order = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_output_order"
      (fun tmpdir ->
        let first_pkg = make_package ~root:tmpdir ~name:"util" ~value:"1" in
        let second_pkg = make_package ~root:tmpdir ~name:"demo" ~value:"2" in
        let third_pkg = make_package ~root:tmpdir ~name:"extra" ~value:"3" in
        let output =
          Riot_build.Output.of_build_results
            [
              make_build_result
                ~package:first_pkg
                ~status:(Riot_executor.Package_builder.Cached (make_artifact "util"));
              make_build_result
                ~package:second_pkg
                ~status:(Riot_executor.Package_builder.Built (make_artifact "demo"));
              make_build_result
                ~package:third_pkg
                ~status:(Riot_executor.Package_builder.Skipped { reason = "later" });
            ]
        in
        Test.assert_equal
          ~expected:[ "util"; "demo"; "extra" ]
          ~actual:(Riot_build.Output.packages output |> List.map ~fn:Riot_build.Output.package_name);
        Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_output_find_package_requires_exact_name = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_output_exact"
      (fun tmpdir ->
        let app_pkg = make_package ~root:tmpdir ~name:"app" ~value:"1" in
        let app_test_pkg = make_package ~root:tmpdir ~name:"app-test" ~value:"2" in
        let output =
          Riot_build.Output.of_build_results
            [
              make_build_result
                ~package:app_test_pkg
                ~status:(Riot_executor.Package_builder.Cached (make_artifact "app-test"));
              make_build_result
                ~package:app_pkg
                ~status:(Riot_executor.Package_builder.Built (make_artifact "app"));
            ]
        in
        match Riot_build.Output.find_package output "app" with
        | Some package_output ->
            Test.assert_equal
              ~expected:"app"
              ~actual:(Riot_build.Output.package_name package_output);
            Ok ()
        | None ->
            Error "expected exact package lookup to find app")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_output_preserves_artifact_payloads = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_output_artifacts"
      (fun tmpdir ->
        let built_pkg = make_package ~root:tmpdir ~name:"built" ~value:"1" in
        let cached_pkg = make_package ~root:tmpdir ~name:"cached" ~value:"2" in
        let built_artifact = make_artifact "built" in
        let cached_artifact = make_artifact "cached" in
        let output =
          Riot_build.Output.of_build_results
            [
              make_build_result
                ~package:built_pkg
                ~status:(Riot_executor.Package_builder.Built built_artifact);
              make_build_result
                ~package:cached_pkg
                ~status:(Riot_executor.Package_builder.Cached cached_artifact);
            ]
        in
        match
          Riot_build.Output.find_package output "built",
          Riot_build.Output.find_package output "cached"
        with
        | Some built_output, Some cached_output -> (
            match
              Riot_build.Output.package_status built_output,
              Riot_build.Output.package_status cached_output
            with
            | Riot_build.Output.Built built, Riot_build.Output.Cached cached ->
                Test.assert_equal
                  ~expected:built_artifact.hash
                  ~actual:built.hash;
                Test.assert_equal
                  ~expected:cached_artifact.hash
                  ~actual:cached.hash;
                Ok ()
            | _ ->
                Error "expected built/cached output statuses to preserve artifacts")
        | _ ->
            Error "expected output lookup to find built and cached packages")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_output_preserves_skip_and_failure_reasons = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_output_reasons"
      (fun tmpdir ->
        let skipped_pkg = make_package ~root:tmpdir ~name:"skipped" ~value:"1" in
        let failed_pkg = make_package ~root:tmpdir ~name:"failed" ~value:"2" in
        let output =
          Riot_build.Output.of_build_results
            [
              make_build_result
                ~package:skipped_pkg
                ~status:(Riot_executor.Package_builder.Skipped { reason = "not requested" });
              make_build_result
                ~package:failed_pkg
                ~status:(
                  Riot_executor.Package_builder.Failed
                    (Riot_executor.Package_builder.ExecutionFailed {
                       message = "compile failed";
                     })
                );
            ]
        in
        match
          Riot_build.Output.find_package output "skipped",
          Riot_build.Output.find_package output "failed"
        with
        | Some skipped_output, Some failed_output -> (
            match
              Riot_build.Output.package_status skipped_output,
              Riot_build.Output.package_status failed_output
            with
            | Riot_build.Output.Skipped skipped, Riot_build.Output.Failed failed ->
                if not (String.equal skipped "not requested") then
                  Error ("expected skipped reason to be preserved, got: " ^ skipped)
                else if not (String.equal failed "Execution failed: compile failed") then
                  Error ("expected failed reason to be normalized, got: " ^ failed)
                else
                  Ok ()
            | _ ->
                Error "expected skipped/failed output statuses to preserve reasons")
        | _ ->
            Error "expected output lookup to find skipped and failed packages")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_error_message_exposes_runtime_error_text = fun _ctx ->
  let message =
    Riot_build.Build_core.build_error_message
      (Riot_build.Build_core.ToolchainInstallFailed {
         target =
           Result.expect
             (Riot_model.Target.from_string "x86_64-unknown-linux-gnu")
             ~msg:"target";
         error = "toolchain missing";
       })
  in
  Test.assert_true (String.contains message "x86_64-unknown-linux-gnu");
  Test.assert_true (String.contains message "toolchain missing");
  Ok ()

let test_build_returns_typed_outputs = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_build"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        match build_request prepared_workspace (make_request ()) with
        | Error err ->
            Error ("expected build to succeed, got: " ^ build_error_message err)
        | Ok output -> (
            Test.assert_equal
              ~expected:[ "demo" ]
              ~actual:(
                Riot_build.Output.packages output
                |> List.map ~fn:Riot_build.Output.package_name
              );
            match Riot_build.Output.find_package output "demo" with
            | None ->
                Error "expected build output for package demo"
            | Some package_output -> (
                match Riot_build.Output.package_status package_output with
                | Riot_build.Output.Built _
                | Riot_build.Output.Cached _ ->
                    Ok ()
                | Riot_build.Output.Skipped reason ->
                    Error ("expected successful package output, got skipped: " ^ reason)
                | Riot_build.Output.Failed message ->
                    Error ("expected successful package output, got failure: " ^ message)
              )
          ))
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_manual_spec_executes_successfully = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_manual_spec"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        let spec =
          Riot_build.Build_spec.make
            ~workspace:prepared_workspace
            ~package_names:[ "demo" ]
            ~targets:(target_set_of_strings [ Riot_model.Target.to_string (Riot_model.Riot_dirs.host_target ()) ])
            ~scope:Riot_build.Build_spec.Runtime
            ~profile:Riot_model.Profile.debug
        in
        match Riot_build.Build_core.build spec with
        | Error err ->
            Error ("expected manual build spec to execute, got: " ^ build_error_message err)
        | Ok output ->
            Test.assert_true
              (Option.is_some (Riot_build.Output.find_package output "demo"));
            Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_manual_build_spec_without_target_names_defaults_to_host = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_empty_targets"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        let spec =
          Riot_build.Build_spec.make
            ~workspace:prepared_workspace
            ~package_names:[ "demo" ]
            ~targets:(Riot_model.Target.Set.empty ())
            ~scope:Riot_build.Build_spec.Runtime
            ~profile:Riot_model.Profile.debug
        in
        match Riot_build.Build_core.build spec with
        | Error err ->
            Error ("expected empty target spec to fall back to host, got: " ^ build_error_message err)
        | Ok output ->
            Test.assert_true
              (Option.is_some (Riot_build.Output.find_package output "demo"));
            Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_manual_build_spec_preserves_exact_target_subset = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_exact_target_subset"
      (fun tmpdir ->
        let host = Riot_model.Target.to_string (Riot_model.Riot_dirs.host_target ()) in
        let workspace =
          make_valid_workspace
            ~toolchain_targets:[ host; "x86_64-unknown-linux-gnu"; "wasm32-unknown-wasi" ]
            tmpdir
        in
        let prepared_workspace = make_prepared_workspace workspace in
        let requested_targets = [ host; "x86_64-unknown-linux-gnu" ] in
        let spec =
          Riot_build.Build_spec.make
            ~workspace:prepared_workspace
            ~package_names:[ "demo" ]
            ~targets:(target_set_of_strings requested_targets)
            ~scope:Riot_build.Build_spec.Runtime
            ~profile:Riot_model.Profile.debug
        in
        let target_count = ref None in
        let _ =
          Riot_build.Build_core.build
            ~on_event:(function
              | Riot_build.Event.Phase
                  (Riot_build.Event.RuntimePhase
                     (Riot_build.Event.TargetsResolved { target_count = count }))
                ->
                  target_count := Some count
              | _ ->
                  ())
            spec
        in
        match !target_count with
        | Some count ->
            Test.assert_equal
              ~expected:(List.length requested_targets)
              ~actual:count;
            Ok ()
        | None ->
            Error "expected build to emit a targets_resolved event")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_output_lookup_returns_none_for_missing_package = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_missing_output_lookup"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        match build_request prepared_workspace (make_request ()) with
        | Error err ->
            Error ("expected build to succeed, got: " ^ build_error_message err)
        | Ok output ->
            Test.assert_true
              (Option.is_none (Riot_build.Output.find_package output "missing"));
            Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_returns_outputs_for_all_requested_packages = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_multi_output"
      (fun tmpdir ->
        let workspace =
          make_valid_workspace ~package_names:[ "demo"; "util" ] tmpdir
        in
        let prepared_workspace = make_prepared_workspace workspace in
        match
          build_request prepared_workspace (make_request ~packages:[ "demo"; "util" ] ())
        with
        | Error err ->
            Error ("expected build to succeed, got: " ^ build_error_message err)
        | Ok output ->
            Test.assert_equal
              ~expected:[ "demo"; "util" ]
              ~actual:(
                [ Riot_build.Output.find_package output "demo";
                  Riot_build.Output.find_package output "util" ]
                |> List.filter_map ~fn:(function
                     | Some package_output -> Some package_output
                     | None -> None)
                |> List.map ~fn:Riot_build.Output.package_name
                |> List.sort ~compare:String.compare
              );
            Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_can_return_cached_outputs_on_repeat_builds = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_cached_build"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        let request = make_request () in
        let _ =
          build_request prepared_workspace request
          |> Result.expect ~msg:"expected first build to succeed"
        in
        match build_request prepared_workspace request with
        | Error err ->
            Error ("expected second build to succeed, got: " ^ build_error_message err)
        | Ok output -> (
            match Riot_build.Output.find_package output "demo" with
            | Some package_output -> (
                match Riot_build.Output.package_status package_output with
                | Riot_build.Output.Cached _ -> Ok ()
                | Riot_build.Output.Built _ ->
                    Error "expected repeated build to be cached"
                | Riot_build.Output.Skipped reason ->
                    Error ("expected cached package output, got skipped: " ^ reason)
                | Riot_build.Output.Failed message ->
                    Error ("expected cached package output, got failure: " ^ message)
              )
            | None ->
                Error "expected build output for package demo"
          ))
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_cached_build_does_not_emit_generation_recording_events = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_cached_build_events"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        let request = make_request () in
        let _ =
          build_request prepared_workspace request
          |> Result.expect ~msg:"expected first build to succeed"
        in
        let seen_generation_event = ref false in
        let result =
          build_request
            ~on_event:(function
              | Riot_build.Phase
                  (Riot_build.Event.RuntimePhase
                    (Riot_build.Event.CacheGenerationRecordingStarted _
                    | Riot_build.Event.CacheGenerationRecorded _)) ->
                  seen_generation_event := true
              | _ ->
                  ())
            prepared_workspace
            request
        in
        match result with
        | Error err ->
            Error ("expected second build to succeed, got: " ^ build_error_message err)
        | Ok _ ->
            Test.assert_false !seen_generation_event;
            Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_resolve_reports_target_misses_before_build = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_target_miss"
      (fun tmpdir ->
        let workspace =
          make_valid_workspace ~toolchain_targets:[ "aarch64-apple-darwin" ] tmpdir
        in
        let prepared_workspace = make_prepared_workspace workspace in
        let request =
          make_request ~targets:(Riot_model.Target.parse "linux") ()
        in
        match resolve_request prepared_workspace request with
        | Error (Riot_build.Build_core.TargetSelectionFailed { pattern; _ }) when String.equal pattern "linux" ->
            Ok ()
        | Error err ->
            Error ("expected typed target-miss error, got: " ^ Riot_build.Build_core.resolve_error_message err)
        | Ok _ ->
            Error "expected resolution to fail before build execution")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_emits_runtime_phases_in_order = fun _ctx ->
  match
    Fs.with_tempdir
      ~prefix:"riot_build_core_events"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let prepared_workspace = make_prepared_workspace workspace in
        let seen = ref [] in
        let result =
          build_request
            ~on_event:(function
              | Riot_build.Phase (Riot_build.Event.RuntimePhase phase) ->
                  seen := !seen @ [ phase_name phase ]
              | Riot_build.Pm _
              | Riot_build.BuildingTarget _
              | Riot_build.CacheGc _
              | Riot_build.Streaming _
              | Riot_build.Phase (Riot_build.Event.CliPhase _) ->
                  ())
            prepared_workspace
            (make_request ())
        in
        match result with
        | Error err ->
            Error ("expected build to succeed, got: " ^ build_error_message err)
        | Ok _ ->
            expect_subsequence
              ~haystack:!seen
              ~needle:[
                "targets_resolved";
                "toolchains_ensured";
                "toolchains_validated";
                "client_connecting";
                "client_connected";
                "target_build_started";
                "target_build_finished";
                "returning_results";
              ])
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let tests =
  let open Test in
  [
    case
      "build core: prepared workspace exposes member packages"
      test_prepared_workspace_exposes_member_packages;
    case
      "build core: prepared workspace preserves workspace manager"
      test_prepared_workspace_preserves_workspace_manager;
    case
      "build core: prepared workspace defaults workspace manager to none"
      test_prepared_workspace_defaults_workspace_manager_to_none;
    case
      "build core: prepared workspace preserves workspace package order"
      test_prepared_workspace_preserves_workspace_package_order;
    case
      "build core: request preserves inputs"
      test_request_preserves_inputs;
    case
      "build core: request preserves host target selector"
      test_request_preserves_host_target_selector;
    case
      "build core: target selector from_string normalizes aliases"
      test_target_selector_from_string_normalizes_aliases;
    case
      "build core: target selector CLI parsing prefers all-targets"
      test_target_selector_of_cli_options_prefers_all_targets;
    case
      "build core: build spec preserves inputs"
      test_build_spec_preserves_inputs;
    case
      "build core: target selector resolves exact targets"
      test_target_selector_resolve_exact_targets;
    case
      "build core: target selector resolves substring patterns"
      test_target_selector_resolve_substring_patterns;
    case
      "build core: target selector configured targets default to host and preserve config"
      test_target_selector_configured_targets_default_to_host_and_preserve_config;
    case
      "build core: resolve returns a typed build spec"
      test_resolve_returns_a_typed_build_spec;
    case
      "build core: top-level resolve matches build core"
      test_top_level_resolve_matches_build_core;
    case
      "build core: resolve preserves scope and profile"
      test_resolve_preserves_scope_and_profile;
    case
      "build core: resolve uses all packages when none are requested"
      test_resolve_uses_all_packages_when_none_are_requested;
    case
      "build core: resolve all packages sorts available package names"
      test_resolve_all_packages_sorts_available_package_names;
    case
      "build core: resolve uses all configured targets"
      test_resolve_uses_all_configured_targets;
    case
      "build core: resolve matches configured target patterns"
      test_resolve_matches_configured_target_patterns;
    case
      "build core: resolve preserves requested package order"
      test_resolve_preserves_requested_package_order;
    case
      "build core: resolve maps native alias to host target"
      test_resolve_native_alias_uses_host_target;
    case
      "build core: resolve defaults all targets to host when config is absent"
      test_resolve_all_without_explicit_targets_uses_host;
    case
      "build core: resolve reports missing package before build"
      test_resolve_reports_missing_package_before_build;
    case
      "build core: resolve reports multiple missing packages before build"
      test_resolve_reports_multiple_missing_packages_before_build;
    case
      "build core: resolve prefers missing packages over target misses"
      test_resolve_prefers_missing_packages_over_target_misses;
    case
      "build core: resolve error messages include missing and available packages"
      test_resolve_error_message_lists_missing_packages;
    case
      "build core: resolve target miss messages include pattern and available targets"
      test_resolve_error_message_lists_target_pattern_and_available_targets;
    case
      "build core: output maps executor statuses"
      test_output_maps_executor_statuses;
    case
      "build core: output preserves build result order"
      test_output_preserves_build_result_order;
    case
      "build core: output lookup requires exact package names"
      test_output_find_package_requires_exact_name;
    case
      "build core: output preserves artifact payloads"
      test_output_preserves_artifact_payloads;
    case
      "build core: output preserves skip and failure reasons"
      test_output_preserves_skip_and_failure_reasons;
    case
      "build core: build error messages expose runtime text"
      test_build_error_message_exposes_runtime_error_text;
    case
      "build core: build returns typed outputs"
      test_build_returns_typed_outputs;
    case
      "build core: manual build spec executes successfully"
      test_build_manual_spec_executes_successfully;
    case
      "build core: manual build spec without target names defaults to host"
      test_manual_build_spec_without_target_names_defaults_to_host;
    case
      "build core: manual build spec preserves exact target subset"
      test_manual_build_spec_preserves_exact_target_subset;
    case
      "build core: output lookup returns none for missing packages"
      test_build_output_lookup_returns_none_for_missing_package;
    case
      "build core: build returns outputs for all requested packages"
      test_build_returns_outputs_for_all_requested_packages;
    case
      "build core: repeated builds can return cached outputs"
      test_build_can_return_cached_outputs_on_repeat_builds;
    case
      "build core: cached builds do not emit generation recording events"
      test_cached_build_does_not_emit_generation_recording_events;
    case
      "build core: resolve reports target misses before build"
      test_resolve_reports_target_misses_before_build;
    case
      "build core: build emits runtime phases in order"
      test_build_emits_runtime_phases_in_order;
  ]

let name = "Riot Build Core Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
