open Std
module Test = Std.Test
module Build_spec = Riot_build.Internal.Build_spec
module Build_runtime = Riot_build.Internal.Build_runtime
module Package_builder = Riot_build.Internal.Package_builder

let package_name = fun name ->
  Riot_model.Package_name.from_string name |> Result.expect ~msg:("invalid package name: " ^ name)

let target = fun value ->
  Riot_model.Target.from_string value |> Result.expect ~msg:("invalid target triple: " ^ value)

let write_workspace_manifest = fun ~root ~members ->
  let members = members
  |> List.map ~fn:(fun member -> "  \"" ^ Path.to_string member ^ "\"")
  |> String.concat ",\n" in
  let content = "[workspace]\nmembers = [\n" ^ members ^ "\n]\n" in
  Fs.write content Path.(root / Path.v "riot.toml") |> Result.expect ~msg:"Write workspace riot.toml failed"

let write_toolchain_config = fun ~root ~targets ->
  let target_lines = targets |> List.map ~fn:(fun target -> "\"" ^ target ^ "\"") |> String.concat ", " in
  Fs.write
    ("[toolchain]\nversion = \""
    ^ Riot_model.Toolchain_config.default_ocaml_version
    ^ "\"\ntargets = ["
    ^ target_lines
    ^ "]\n")
    Path.(root / Path.v "ocaml-toolchain.toml")
  |> Result.expect ~msg:"Write ocaml-toolchain.toml failed"

let make_package = fun ~root ~name ~source ->
  let pkg_dir = Path.(root / Path.v name) in
  let package_name = package_name name in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  Fs.create_dir_all src_dir |> Result.expect ~msg:"Create src failed";
  Fs.write source Path.(src_dir / Path.v "lib.ml") |> Result.expect ~msg:"Write source failed";
  Fs.write
    ("[package]\nname = \"" ^ name ^ "\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n")
    Path.(pkg_dir / Path.v "riot.toml")
  |> Result.expect ~msg:"Write riot.toml failed";
  Riot_model.Package.make ~name:package_name ~path:pkg_dir ~relative_path:(Path.v name) ~library:{
    path = Path.v "src/lib.ml"
  }
    ~sources:{
      src = [ Path.v "src/lib.ml" ];
      native = [];
      tests = [];
      examples = [];
      bench = [];
    }
    ()

let make_workspace = fun ?target_dir ?toolchain_targets ~root ~packages () ->
  write_workspace_manifest
    ~root
    ~members:(List.map packages ~fn:(fun (pkg: Riot_model.Package.t) -> pkg.relative_path));
  (match toolchain_targets with
  | Some targets -> write_toolchain_config ~root ~targets
  | None -> ());
  Riot_model.Workspace.make_realized ~root ?target_dir ~packages ()

let make_valid_workspace = fun ?target_dir ?toolchain_targets tmpdir ->
  let package = make_package ~root:tmpdir ~name:"demo" ~source:"let value = 42\n" in
  make_workspace ?target_dir ?toolchain_targets ~root:tmpdir ~packages:[ package ] ()

let make_workspace_with_sources = fun ?toolchain_targets ~root ~packages () ->
  let packages =
    List.map packages
      ~fn:(fun (name, source) -> make_package ~root ~name ~source)
  in
  make_workspace ?toolchain_targets ~root ~packages ()

let make_request = fun ~workspace ?(profile = Riot_model.Profile.debug) () ->
  Riot_build.Request.make
    ~workspace
    ~packages:[ package_name "demo" ]
    ~targets:Riot_model.Target.Host
    ~scope:Riot_build.Request.Runtime
    ~profile
    ()

let build_request = fun request -> Riot_build.build request

let write_nested_udp_workspace = fun ~root ~creation_order ->
  let pkg_dir = Path.(root / Path.v "demo") in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let net_dir = Path.(src_dir / Path.v "net") in
  Fs.create_dir_all net_dir |> Result.expect ~msg:"Create nested src failed";
  write_workspace_manifest ~root ~members:[ Path.v "demo" ];
  Fs.write
    "[package]\nname = \"demo\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/demo.ml\"\n"
    Path.(pkg_dir / Path.v "riot.toml")
  |> Result.expect ~msg:"Write nested riot.toml failed";
  let file_for_key = function
    | "demo_ml" -> (Path.(src_dir / Path.v "demo.ml"), "module Net = Net\n")
    | "net_ml" -> (
      Path.(net_dir / Path.v "net.ml"),
      "module Udp_socket = Udp_socket\nmodule Udp_server = Udp_server\n"
    )
    | "udp_socket_mli" -> (Path.(net_dir / Path.v "udp_socket.mli"), "type t\n")
    | "udp_socket_ml" -> (Path.(net_dir / Path.v "udp_socket.ml"), "type t = unit\n")
    | "udp_server_mli" -> (
      Path.(net_dir / Path.v "udp_server.mli"),
      "type handler = socket:Udp_socket.t -> bytes -> unit\nval run : handler -> unit\n"
    )
    | "udp_server_ml" -> (
      Path.(net_dir / Path.v "udp_server.ml"),
      "type handler = socket:Udp_socket.t -> bytes -> unit\nlet run _ = ()\n"
    )
    | key -> panic ("unknown nested udp workspace file key: " ^ key)
  in
  List.for_each creation_order
    ~fn:(fun key ->
      let path, contents = file_for_key key in
      Fs.write contents path |> Result.expect ~msg:("Write nested source failed: " ^ key))

let test_release_build_uses_release_lane = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"riot_build_release_runtime"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let host_target = Riot_model.Riot_dirs.host_target () in
        let release_package_dir = Riot_model.Riot_dirs.out_dir_in_workspace
          ~workspace
          ~profile:"release"
          ~target:host_target
        |> fun out_dir -> Path.(out_dir / Path.v "demo") in
        let debug_package_dir = Riot_model.Riot_dirs.out_dir_in_workspace
          ~workspace
          ~profile:"debug"
          ~target:host_target
        |> fun out_dir -> Path.(out_dir / Path.v "demo") in
        match build_request (make_request ~workspace ~profile:Riot_model.Profile.release ()) with
        | Error err -> Error ("expected release build to succeed, got: " ^ Riot_build.error_message err)
        | Ok _ ->
            if not
                (Fs.exists release_package_dir |> Result.unwrap_or ~default:false) then
              Error ("expected release output under " ^ Path.to_string release_package_dir)
            else if Fs.exists debug_package_dir |> Result.unwrap_or ~default:false then
              Error ("did not expect debug output under " ^ Path.to_string debug_package_dir)
            else
              Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_respects_custom_target_dir = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"riot_build_custom_target_runtime"
      (fun tmpdir ->
        let workspace = make_valid_workspace ~target_dir:"build-out" tmpdir in
        let host_target = Riot_model.Riot_dirs.host_target () in
        let release_package_dir = Riot_model.Riot_dirs.out_dir_in_workspace
          ~workspace
          ~profile:"release"
          ~target:host_target
        |> fun out_dir -> Path.(out_dir / Path.v "demo") in
        let default_release_dir = Riot_model.Riot_dirs.out_dir_with_target
          ~workspace_root:workspace.root
          ~profile:"release"
          ~target:host_target
        |> fun out_dir -> Path.(out_dir / Path.v "demo") in
        match build_request (make_request ~workspace ~profile:Riot_model.Profile.release ()) with
        | Error err -> Error ("expected custom-target build to succeed, got: "
        ^ Riot_build.error_message err)
        | Ok _ ->
            if not
                (Fs.exists release_package_dir |> Result.unwrap_or ~default:false) then
              Error ("expected release output under custom target dir " ^ Path.to_string release_package_dir)
            else if Fs.exists default_release_dir |> Result.unwrap_or ~default:false then
              Error ("did not expect output under default build dir " ^ Path.to_string default_release_dir)
            else
              Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_nested_udp_workspace_builds_across_file_creation_orders = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"riot_build_nested_udp_runtime"
      (fun tmpdir ->
        let orders = [
          (
            "canonical",
            [
              "demo_ml";
              "net_ml";
              "udp_socket_mli";
              "udp_socket_ml";
              "udp_server_mli";
              "udp_server_ml"
            ]
          );
          (
            "server_first",
            [
              "udp_server_mli";
              "udp_server_ml";
              "udp_socket_mli";
              "udp_socket_ml";
              "net_ml";
              "demo_ml"
            ]
          );
          (
            "socket_impl_first",
            [
              "udp_socket_ml";
              "net_ml";
              "demo_ml";
              "udp_server_ml";
              "udp_socket_mli";
              "udp_server_mli"
            ]
          );
          (
            "mixed",
            [
              "net_ml";
              "udp_server_mli";
              "demo_ml";
              "udp_socket_mli";
              "udp_server_ml";
              "udp_socket_ml"
            ]
          );
        ] in
        let rec run = function
          | [] -> Ok ()
          | (order_name, creation_order) :: rest ->
              let root = Path.(tmpdir / Path.v order_name) in
              Fs.create_dir_all root |> Result.expect ~msg:"Create order root failed";
              write_nested_udp_workspace ~root ~creation_order;
              let workspace_manager = Riot_model.Workspace_manager.create () in
              begin
                match Riot_model.Workspace_manager.scan workspace_manager root with
                | Error err -> Error ("workspace scan failed for creation order "
                ^ order_name
                ^ ": "
                ^ err)
                | Ok (workspace, load_errors) ->
                    if not (List.is_empty load_errors) then
                      Error ("workspace scan had load errors for creation order " ^ order_name)
                    else
                      let registry = Pkgs_ml.Registry.create_filesystem
                        ?riot_home:None
                        ~registry_name:"pkgs.ml"
                        ()
                      |> Result.expect ~msg:"registry init failed" in
                      let prepared_workspace = Riot_deps.ensure_workspace
                        ~workspace_manager
                        ~mode:Riot_deps.Dep_solver.Refresh
                        ~registry
                        ~workspace
                        ()
                      |> Result.expect ~msg:"workspace prepare failed" in
                      match build_request (make_request ~workspace:prepared_workspace ()) with
                      | Ok _ -> run rest
                      | Error err -> Error ("nested udp build failed for creation order "
                      ^ order_name
                      ^ ": "
                      ^ Riot_build.error_message err)
              end
        in
        run orders)
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_execute_rejects_invalid_parallelism = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"riot_build_invalid_parallelism_runtime"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let spec = Build_spec.make
          ~workspace
          ~package_names:[ package_name "demo" ]
          ~targets:(Riot_model.Target.make_set [ Riot_model.Target.current ])
          ~scope:Riot_build.Request.Runtime
          ~profile:Riot_model.Profile.debug
          ~requested_parallelism:(Some 0) in
        match Riot_build.Internal.Build_runtime.execute spec with
        | Error (Build_runtime.InvalidRequestedParallelism 0) -> Ok ()
        | Error err ->
            Error ("expected invalid parallelism error, got: " ^ Build_runtime.error_message err)
        | Ok _ ->
            Error "expected invalid parallelism to reject execution")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_execute_does_not_record_cache_generation_when_disabled = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"riot_build_no_cache_recording"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let spec = Build_spec.make
          ~workspace
          ~package_names:[ package_name "demo" ]
          ~targets:(Riot_model.Target.make_set [ Riot_model.Target.current ])
          ~scope:Riot_build.Request.Runtime
          ~profile:Riot_model.Profile.debug
          ~requested_parallelism:None in
        let saw_cache_event = ref false in
        match
          Riot_build.Internal.Build_runtime.execute
            ~record_cache_generation:false
            ~on_event:(fun event ->
              match event with
              | Build_runtime.Phase (Riot_build.Event.CacheGenerationRecordingStarted _)
              | Build_runtime.Phase (Riot_build.Event.CacheGenerationRecorded _) -> saw_cache_event := true
              | _ -> ())
            spec
        with
        | Error err -> Error ("expected build to succeed, got: " ^ Build_runtime.error_message err)
        | Ok _ ->
            Test.assert_false !saw_cache_event;
            Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_execute_partial_failures_by_default = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"riot_build_default_partial_failure"
      (fun tmpdir ->
        let workspace =
          make_workspace_with_sources
            ~root:tmpdir
            ~packages:[
              ( "good", "let value = 1\n" );
              ( "bad", "let broken =" );
            ] () in
        let bad = package_name "bad" in
        let spec = Build_spec.make
          ~workspace
          ~package_names:[ package_name "good"; package_name "bad" ]
          ~targets:(Riot_model.Target.make_set [ Riot_model.Target.current ])
          ~scope:Riot_build.Request.Runtime
          ~profile:Riot_model.Profile.debug
          ~requested_parallelism:(Some 1) in
        let saw_returning_results = ref false in
        let saw_partial_failure = ref false in
        match
          Riot_build.Internal.Build_runtime.execute
            ~on_event:(fun event ->
              match event with
              | Build_runtime.Phase (Riot_build.Event.TargetBuildFinished { had_partial_failure }) ->
                  saw_partial_failure := had_partial_failure
              | Build_runtime.Phase (Riot_build.Event.ReturningResults _) ->
                  saw_returning_results := true
              | _ -> ())
            spec
        with
        | Ok _ -> Error "expected default build to fail when any package fails"
        | Error (Build_runtime.BuildFailed { errors }) ->
            let output = Riot_build.Build_result.of_build_results errors in
            let bad_output = Riot_build.Build_result.find_package output bad in
            let has_bad_failure =
              match bad_output with
              | Some bad_package ->
                  (match Riot_build.Build_result.package_status bad_package with
                  | Riot_build.Build_result.Failed _ -> true
                  | _ -> false)
              | None -> false
            in
            if not has_bad_failure then
              Error "expected failure output to include failed bad package"
            else if not !saw_partial_failure then
              Error "expected target build finished event to report partial failure"
            else if !saw_returning_results then
              Error "expected returning results event not to fire on build failure"
            else
              Ok ()
        | Error err -> Error ("expected BuildFailed, got: " ^ Build_runtime.error_message err))
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_execute_allows_partial_failures = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"riot_build_allow_partial_failures"
      (fun tmpdir ->
        let workspace =
          make_workspace_with_sources
            ~root:tmpdir
            ~packages:[
              ( "good", "let value = 2\n" );
              ( "bad", "let broken =" );
            ] () in
        let good = package_name "good" in
        let bad = package_name "bad" in
        let spec = Build_spec.make
          ~workspace
          ~package_names:[ good; bad ]
          ~targets:(Riot_model.Target.make_set [ Riot_model.Target.current ])
          ~scope:Riot_build.Request.Runtime
          ~profile:Riot_model.Profile.debug
          ~requested_parallelism:(Some 1) in
        let result =
          Riot_build.Internal.Build_runtime.execute
            ~allow_partial_failures:true
            ~on_event:(fun _ -> ())
            spec
        in
        match result with
        | Error err ->
            Error ("expected partial failures to be returned, got: " ^ Build_runtime.error_message err)
        | Ok results ->
            let build_output = Riot_build.Build_result.of_build_results results in
            let good_result = Riot_build.Build_result.find_package build_output good in
            let bad_result = Riot_build.Build_result.find_package build_output bad in
            match good_result with
            | None -> Error "expected good package result"
            | Some good_result ->
                match Riot_build.Build_result.package_status good_result with
                | Riot_build.Build_result.Built _
                | Riot_build.Build_result.Cached _ -> (
                    match bad_result with
                    | None -> Error "expected bad package result"
                    | Some bad_result ->
                        (match Riot_build.Build_result.package_status bad_result with
                        | Riot_build.Build_result.Failed _ -> Ok ()
                        | _ ->
                            Error "expected bad package result to be failed with allow_partial_failures"))
                | Riot_build.Build_result.Skipped _
                | Riot_build.Build_result.Failed _ -> Error "expected good package result to be successful"
  )
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_execute_allows_multi_target_partial_failures = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"riot_build_allow_partial_failures_multi_target"
      (fun tmpdir ->
        let host_target = Riot_model.Target.current in
        let secondary_target =
          if String.equal (Riot_model.Target.to_string host_target) "x86_64-unknown-linux-gnu" then
            target "aarch64-unknown-linux-gnu"
          else
            target "x86_64-unknown-linux-gnu"
        in
        let requested_targets = Riot_model.Target.Set.of_list [ host_target; secondary_target ] in
        let expected_targets =
          requested_targets
          |> Riot_model.Target.Set.to_list
          |> List.map ~fn:Riot_model.Target.to_string
          |> List.sort ~compare:String.compare
        in
        let expected_target_count = List.length expected_targets in
        let workspace =
          make_workspace_with_sources
            ~root:tmpdir
            ~toolchain_targets:expected_targets
            ~packages:[
              ( "good", "let value = 2\n" );
              ( "bad", "let broken =" );
            ] () in
        let good = package_name "good" in
        let bad = package_name "bad" in
        let spec = Build_spec.make
          ~workspace
          ~package_names:[ good; bad ]
          ~targets:requested_targets
          ~scope:Riot_build.Request.Runtime
          ~profile:Riot_model.Profile.debug
          ~requested_parallelism:(Some 1) in
        let started_targets = ref [] in
        let finished_targets = ref [] in
        let finished_counts = ref [] in
        let partial_flags = ref [] in
        match
          Riot_build.Internal.Build_runtime.execute
            ~allow_partial_failures:true
            ~on_event:(fun event ->
              match event with
              | Build_runtime.Phase (Riot_build.Event.TargetBuildStarted { target }) ->
                  started_targets := Riot_model.Target.to_string target :: !started_targets
              | Build_runtime.Phase (Riot_build.Event.TargetBuildFinished {
                target;
                result_count;
                had_partial_failure = partial
              }) ->
                  finished_targets := Riot_model.Target.to_string target :: !finished_targets;
                  finished_counts := result_count :: !finished_counts;
                  partial_flags := partial :: !partial_flags
              | _ -> ())
            spec
        with
        | Error err ->
            Error ("expected partial failures to be returned, got: " ^ Build_runtime.error_message err)
        | Ok results ->
            if not (Int.equal (List.length !started_targets) expected_target_count) then
              Error ("expected " ^ Int.to_string expected_target_count ^ " target builds started")
            else if not (Int.equal (List.length !finished_targets) expected_target_count) then
              Error ("expected " ^ Int.to_string expected_target_count ^ " target builds finished")
            else if not (Int.equal (List.length !finished_counts) expected_target_count) then
              Error ("expected " ^ Int.to_string expected_target_count ^ " target result_count events")
            else if not (List.all !finished_counts ~fn:(fun count -> count = 2)) then
              Error ("expected each lane to report two package results, got "
              ^ String.concat ", " (List.map !finished_counts ~fn:Int.to_string))
            else if not (List.all !partial_flags ~fn:(fun partial -> partial)) then
              Error "expected each lane to report partial failures"
            else
              let sort_target_names = List.sort ~compare:String.compare in
              let started_sorted = sort_target_names !started_targets in
              let finished_sorted = sort_target_names !finished_targets in
              let rec equal_target_lists = fun left right ->
                match left, right with
                | [], [] -> true
                | left :: left_rest, right :: right_rest ->
                    String.equal left right && equal_target_lists left_rest right_rest
                | _, _ -> false
              in
              if not (equal_target_lists expected_targets started_sorted) then
                Error ("expected target starts to include " ^ String.concat ", " expected_targets)
              else if not (equal_target_lists expected_targets finished_sorted) then
                Error ("expected target finishes to include " ^ String.concat ", " expected_targets)
              else
                let build_output = Riot_build.Build_result.of_build_results results in
                let good_output = Riot_build.Build_result.find_package build_output good in
                let bad_output = Riot_build.Build_result.find_package build_output bad in
                let good_ok =
                  match good_output with
                  | Some package_output ->
                      (match Riot_build.Build_result.package_status package_output with
                      | Riot_build.Build_result.Built _
                      | Riot_build.Build_result.Cached _ -> true
                      | _ -> false)
                  | None -> false
                in
                let bad_ok =
                  match bad_output with
                  | Some package_output ->
                      (match Riot_build.Build_result.package_status package_output with
                      | Riot_build.Build_result.Failed _ -> true
                      | _ -> false)
                  | None -> false
                in
                if not good_ok then
                  Error "expected merged output to include successful good package"
                else if not bad_ok then
                  Error "expected merged output to include failed bad package"
                else
                  Ok ()
      )
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_execute_multi_target_reports_global_returning_results = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"riot_build_multi_target_returning_results"
      (fun tmpdir ->
        let host_target = Riot_model.Target.current in
        let secondary_target =
          if String.equal (Riot_model.Target.to_string host_target) "x86_64-unknown-linux-gnu" then
            target "aarch64-unknown-linux-gnu"
          else
            target "x86_64-unknown-linux-gnu"
        in
        let requested_targets = Riot_model.Target.Set.of_list [ host_target; secondary_target ] in
        let target_count = List.length (Riot_model.Target.Set.to_list requested_targets) in
        let expected_return_count = target_count * 2 in
        let returning_event = ref None in
        let workspace =
          make_workspace_with_sources
            ~root:tmpdir
            ~toolchain_targets:(Riot_model.Target.Set.to_list requested_targets
              |> List.map ~fn:Riot_model.Target.to_string)
            ~packages:[
              ( "good", "let value = 2\n" );
              ( "bad", "let broken =" );
            ] () in
        let spec = Build_spec.make
          ~workspace
          ~package_names:[ package_name "good"; package_name "bad" ]
          ~targets:requested_targets
          ~scope:Riot_build.Request.Runtime
          ~profile:Riot_model.Profile.debug
          ~requested_parallelism:(Some 1) in
        match
          Riot_build.Internal.Build_runtime.execute
            ~allow_partial_failures:true
            ~on_event:(fun event ->
              match event with
              | Build_runtime.Phase (Riot_build.Event.ReturningResults { result_count; had_partial_failure }) ->
                  returning_event := Some (result_count, had_partial_failure)
              | _ -> ())
            spec
        with
        | Error err ->
            Error ("expected partial failures to be returned, got: " ^ Build_runtime.error_message err)
        | Ok _ ->
            match !returning_event with
            | None -> Error "expected returning results event"
            | Some (result_count, had_partial_failure) ->
                if not (Int.equal result_count expected_return_count) then
                  Error
                    ("expected returning result count "
                    ^ Int.to_string expected_return_count
                    ^ ", got "
                    ^ Int.to_string result_count)
                else if not had_partial_failure then
                  Error "expected returning results to report partial failure"
                else
                  Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let tests =
  let open Test in [
    case "build runtime: release builds use the release lane" test_release_build_uses_release_lane;
    case "build runtime: custom target_dir is respected" test_build_respects_custom_target_dir;
    case "build runtime: nested udp workspace succeeds across file creation orders" test_nested_udp_workspace_builds_across_file_creation_orders;
    case "build runtime: rejects invalid parallelism" test_execute_rejects_invalid_parallelism;
    case "build runtime: execute does not record cache generation when disabled"
      test_execute_does_not_record_cache_generation_when_disabled;
    case "build runtime: partial failures fail by default" test_execute_partial_failures_by_default;
    case "build runtime: allow partial failures returns partial results"
      test_execute_allows_partial_failures;
    case "build runtime: multi-target partial failures can succeed with allow flag"
      test_execute_allows_multi_target_partial_failures;
    case "build runtime: multi-target partial build returns aggregated returning results"
      test_execute_multi_target_reports_global_returning_results;
  ]

let name = "Riot Build Runtime Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
