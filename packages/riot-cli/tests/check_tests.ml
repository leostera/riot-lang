open Std
module Test = Std.Test

let with_tempdir_result = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let write_file = fun path content ->
  match Fs.create_dir_all (Path.dirname path) with
  | Error err -> Error (IO.error_message err)
  | Ok () -> (
      match Fs.write content path with
      | Ok () -> Ok ()
      | Error err -> Error (IO.error_message err)
    )

let path_error_message = function
  | Path.InvalidUtf8 { path } -> "invalid utf-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } -> "system returned invalid utf-8 for "
  ^ syscall
  ^ ": "
  ^ path
  | Path.SystemError message -> message

let with_current_dir_result = fun dir fn ->
  match Env.current_dir () with
  | Error err -> Error (path_error_message err)
  | Ok original ->
      match Env.set_current_dir dir with
      | Error err -> Error (path_error_message err)
      | Ok () ->
          let restore () =
            match Env.set_current_dir original with
            | Ok () -> ()
            | Error _ -> ()
          in
          (
            try
              let result = fn () in
              let () = restore () in
              result
            with
            | exn ->
                let () = restore () in
                raise exn
          )

let make_capture_writer = fun () ->
  let chunks = ref [] in
  ((fun chunk -> chunks := chunk :: !chunks), fun () -> !chunks |> List.rev |> String.concat "")

let yellow_bold = "\027[1;33m"

let reset = "\027[0m"

let package_progress_line = fun label package_name ->
  let padding = String.make (Int.max 0 (12 - String.length label)) ' ' in
  padding ^ yellow_bold ^ label ^ reset ^ " " ^ package_name ^ "\n"

let parse_jsonl = fun output ->
  output
  |> String.split_on_char '\n'
  |> List.filter (fun line -> not (String.equal line ""))
  |> List.map (fun line -> Data.Json.of_string line |> Result.expect ~msg:"parse json line")

let parse_check = fun args ->
  match ArgParser.get_matches Riot_cli.Check_cmd.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let make_dependency = fun name ->
  Riot_model.Package.{
    name;
    source =
      {
        workspace = true;
        builtin = false;
        path = None;
        source_locator = None;
        ref_ = None;
        version = Some Std.Version.any;
      };
  }

let empty_sources: Riot_model.Package.sources = {
  src = [];
  native = [];
  tests = [];
  examples = [];
  bench = [];
}

let make_package = fun ~name ~path ~relative_path ?(dependencies = []) ?library ?(sources = empty_sources) () ->
  Riot_model.Package.make ~name ~path ~relative_path ~dependencies ?library ~sources ()

let make_workspace = fun workspace_root packages ->
  Riot_model.Workspace.make ~root:workspace_root ~packages ()

let workspace_typ_store = fun (workspace: Riot_model.Workspace.t) ->
  let contentstore = Contentstore.create
    ~root:Path.(workspace.target_dir_root / Path.v "typ-cache")
    ~policy:Contentstore.Policy.default
    () in
  Typ.Store.create contentstore ()

let scan_workspace_result = fun root ->
  let workspace_manager = Riot_model.Workspace_manager.create () in
  match Riot_model.Workspace_manager.scan workspace_manager root with
  | Error err -> Error err
  | Ok (workspace, load_errors) ->
      if List.is_empty load_errors then
        Ok workspace
      else
        Error (load_errors
        |> List.map Riot_model.Workspace_manager.load_error_to_string
        |> String.concat "\n")

let test_check_accepts_json_flag = fun _ctx ->
  match parse_check [ "check"; "--json"; "app.ml" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected --json flag to be parsed"

let test_check_accepts_quiet_flag = fun _ctx ->
  match parse_check [ "check"; "--quiet"; "app.ml" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "quiet" then
        Ok ()
      else
        Error "expected --quiet flag to be parsed"

let test_check_accepts_explain_option = fun _ctx ->
  match parse_check [ "check"; "--explain"; "TYP2001" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "TYP2001") ~actual:(ArgParser.get_one matches "explain");
      Ok ()

let test_check_accepts_package_option = fun _ctx ->
  match parse_check [ "check"; "-p"; "colors" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "colors") ~actual:(ArgParser.get_one matches "package");
      Ok ()

let test_check_accepts_path_argument = fun _ctx ->
  match parse_check [ "check"; "packages/app/src/app.ml" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      let expected = Path.v "packages/app/src/app.ml" in
      Test.assert_equal
        ~expected:[ expected ]
        ~actual:(ArgParser.get_many matches "path" |> List.map Path.v);
      Ok ()

let test_check_accepts_multiple_paths = fun _ctx ->
  match parse_check [ "check"; "packages/app/src/app.ml"; "packages/kernel/src/kernel.ml" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      let expected = [ Path.v "packages/app/src/app.ml"; Path.v "packages/kernel/src/kernel.ml" ] in
      Test.assert_equal ~expected ~actual:(ArgParser.get_many matches "path" |> List.map Path.v);
      Ok ()

let test_check_accepts_no_path_without_explain = fun _ctx ->
  match parse_check [ "check" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:[] ~actual:(ArgParser.get_many matches "path" |> List.map Path.v);
      Ok ()

let test_check_json_streams_single_explicit_file_without_duplicates = fun _ctx ->
  with_tempdir_result "riot_check_json_single_file"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let package_root = Path.(workspace_root / Path.v "packages/demo") in
      let source_path = Path.(package_root / Path.v "src/broken.ml") in
      let workspace = make_workspace
        workspace_root
        [ make_package ~name:"demo" ~path:package_root ~relative_path:(Path.v "packages/demo") () ] in
      match write_file source_path "let x = y\n" with
      | Error err -> Error err
      | Ok () ->
          let matches = parse_check [ "check"; "--json"; Path.to_string source_path ]
          |> Result.expect ~msg:"parse check args" in
          let stdout, stdout_contents = make_capture_writer () in
          let stderr, stderr_contents = make_capture_writer () in
          (
            match Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches with
            | Ok () -> Error "expected check --json to fail for unbound name"
            | Error _ ->
                let events = parse_jsonl (stdout_contents ()) in
                let count_events kind =
                  events
                  |> List.filter
                    (fun json ->
                      match Data.Json.get_field "type" json with
                      | Some (Data.Json.String actual) -> String.equal actual kind
                      | _ -> false)
                  |> List.length
                in
                Test.assert_equal ~expected:1 ~actual:(count_events "check_start");
                Test.assert_equal ~expected:1 ~actual:(count_events "check_file");
                Test.assert_equal ~expected:1 ~actual:(count_events "check_diagnostic");
                Test.assert_equal ~expected:1 ~actual:(count_events "check_summary");
                let file_event =
                  events
                  |> List.find_opt
                    (fun json ->
                      match Data.Json.get_field "type" json with
                      | Some (Data.Json.String "check_file") -> true
                      | _ -> false)
                  |> Option.expect ~msg:"missing check_file event"
                in
                let expected_path = Some (Data.Json.String "packages/demo/src/broken.ml") in
                let actual_path =
                  match Data.Json.get_field "result" file_event with
                  | Some result_json -> Data.Json.get_field "path" result_json
                  | None -> None
                in
                Test.assert_equal ~expected:expected_path ~actual:actual_path;
                Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                Ok ()
          ))

let test_check_human_output_snapshot = fun ctx ->
  with_tempdir_result "riot_check_human_output"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let package_root = Path.(workspace_root / Path.v "packages/demo") in
      let source_path = Path.(package_root / Path.v "src/broken.ml") in
      let workspace = make_workspace
        workspace_root
        [ make_package ~name:"demo" ~path:package_root ~relative_path:(Path.v "packages/demo") () ] in
      match write_file source_path "let x = y\n" with
      | Error err -> Error err
      | Ok () ->
          let matches = parse_check [ "check"; Path.to_string source_path ] |> Result.expect ~msg:"parse check args" in
          let stdout, stdout_contents = make_capture_writer () in
          let stderr, stderr_contents = make_capture_writer () in
          (
            match Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches with
            | Ok () -> Error "expected check to fail for unbound name"
            | Error _ ->
                Test.assert_equal
                  ~expected:(package_progress_line "Checking" "demo")
                  ~actual:(stderr_contents ());
                Test.Snapshot.assert_text ~ctx ~actual:(stdout_contents ())
          ))

let test_check_success_emits_package_progress = fun _ctx ->
  with_tempdir_result "riot_check_success_silent"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let package_root = Path.(workspace_root / Path.v "packages/demo") in
      let source_path = Path.(package_root / Path.v "src/ok.ml") in
      let workspace = make_workspace
        workspace_root
        [ make_package ~name:"demo" ~path:package_root ~relative_path:(Path.v "packages/demo") () ] in
      match write_file source_path "let id x = x\n" with
      | Error err -> Error err
      | Ok () ->
          let matches = parse_check [ "check"; Path.to_string source_path ] |> Result.expect ~msg:"parse check args" in
          let stdout, stdout_contents = make_capture_writer () in
          let stderr, stderr_contents = make_capture_writer () in
          Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"check simple file";
          Test.assert_equal ~expected:"" ~actual:(stdout_contents ());
          Test.assert_equal
            ~expected:(package_progress_line "Checking" "demo")
            ~actual:(stderr_contents ());
          Ok ())

let test_check_package_filter_limits_workspace_scan = fun _ctx ->
  with_tempdir_result "riot_check_package_filter"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let colors_root = Path.(workspace_root / Path.v "packages/colors") in
      let tty_root = Path.(workspace_root / Path.v "packages/tty") in
      let colors_source = Path.(colors_root / Path.v "src/colors.ml") in
      let tty_source = Path.(tty_root / Path.v "src/tty.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package ~name:"colors" ~path:colors_root ~relative_path:(Path.v "packages/colors") ();
          make_package ~name:"tty" ~path:tty_root ~relative_path:(Path.v "packages/tty") ();
        ] in
      match write_file colors_source "let color = missing_color\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file tty_source "let tty = missing_tty\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check [ "check"; "--json"; "-p"; "colors" ] |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              match Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches with
              | Ok () -> Error "expected package-filtered check to fail for unbound name"
              | Error _ ->
                  let events = parse_jsonl (stdout_contents ()) in
                  let file_paths =
                    events
                    |> List.filter_map
                      (fun json ->
                        match Data.Json.get_field "type" json with
                        | Some (Data.Json.String "check_file") -> (
                            match Data.Json.get_field "result" json with
                            | Some result_json -> Data.Json.get_field "path" result_json
                            | None -> None
                          )
                        | _ -> None)
                  in
                  Test.assert_equal
                    ~expected:[ Data.Json.String "packages/colors/src/colors.ml" ]
                    ~actual:file_paths;
                  Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                  Ok ()
        ))

let test_check_json_includes_typ_event_diagnostics = fun _ctx ->
  with_tempdir_result "riot_check_json_typ_events"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let package_root = Path.(workspace_root / Path.v "packages/demo") in
      let source_path = Path.(package_root / Path.v "src/broken.ml") in
      let workspace = make_workspace
        workspace_root
        [ make_package ~name:"demo" ~path:package_root ~relative_path:(Path.v "packages/demo") () ] in
      match write_file source_path "let value = missing_name\n" with
      | Error err -> Error err
      | Ok () ->
          let matches = parse_check [ "check"; "--json"; Path.to_string source_path ]
          |> Result.expect ~msg:"parse check args" in
          let stdout, stdout_contents = make_capture_writer () in
          let stderr, stderr_contents = make_capture_writer () in
          (
            match Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches with
            | Ok () -> Error "expected check --json to fail for unbound name"
            | Error _ ->
                let events = parse_jsonl (stdout_contents ()) in
                let analysis_finish =
                  events
                  |> List.find_opt
                    (fun json ->
                      match Data.Json.get_field "type" json, Data.Json.get_field
                        "typing_diagnostic_count"
                        json with
                      | (Some (Data.Json.String "typ_source_analysis_finish"), Some (Data.Json.Int count)) when count
                      > 0 -> true
                      | _ -> false)
                  |> Option.expect ~msg:"missing errored typ_source_analysis_finish event"
                in
                let typing_diagnostic_count =
                  match Data.Json.get_field "typing_diagnostic_count" analysis_finish with
                  | Some (Data.Json.Int count) -> count
                  | _ -> (-1)
                in
                let typing_diagnostics =
                  match Data.Json.get_field "typing_diagnostics" analysis_finish with
                  | Some (Data.Json.Array diagnostics) -> diagnostics
                  | _ -> []
                in
                let first_diagnostic_id =
                  match typing_diagnostics with
                  | Data.Json.Object fields :: _ -> (
                      match List.assoc_opt "id" fields with
                      | Some (Data.Json.String id) -> Some id
                      | _ -> None
                    )
                  | _ -> None
                in
                let () = Test.assert_equal ~expected:1 ~actual:typing_diagnostic_count in
                let () = Test.assert_equal ~expected:1 ~actual:(List.length typing_diagnostics) in
                let () = Test.assert_equal ~expected:(Some "TYP2001") ~actual:first_diagnostic_id in
                Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                Ok ()
          ))

let test_check_package_filter_handles_hyphenated_package_names = fun _ctx ->
  with_tempdir_result "riot_check_package_filter_hyphenated"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let kernel_root = Path.(workspace_root / Path.v "packages/kernel") in
      let kernel_new_root = Path.(workspace_root / Path.v "packages/kernel-new") in
      let kernel_source = Path.(kernel_root / Path.v "src/kernel.ml") in
      let kernel_new_source = Path.(kernel_new_root / Path.v "src/kernel_new.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package ~name:"kernel" ~path:kernel_root ~relative_path:(Path.v "packages/kernel") ();
          make_package
            ~name:"kernel-new"
            ~path:kernel_new_root
            ~relative_path:(Path.v "packages/kernel-new")
            ();
        ] in
      match write_file kernel_source "let old_runtime = 1\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file kernel_new_source "let new_runtime = 2\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check [ "check"; "--json"; "-p"; "kernel-new" ]
              |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"hyphenated package filter should select kernel-new";
              let events = parse_jsonl (stdout_contents ()) in
              let package_events =
                events
                |> List.filter_map
                  (fun json ->
                    match Data.Json.get_field "type" json, Data.Json.get_field "package_name" json with
                    | Some (Data.Json.String "check_package_planning_start"), Some (Data.Json.String package_name) -> Some package_name
                    | _ -> None)
              in
              let file_paths =
                events
                |> List.filter_map
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_file") -> (
                        match Data.Json.get_field "result" json with
                        | Some result_json -> Data.Json.get_field "path" result_json
                        | None -> None
                      )
                    | _ -> None)
              in
              let () = Test.assert_equal ~expected:[ "kernel-new" ] ~actual:package_events in
              let () = Test.assert_equal
                ~expected:[ Data.Json.String "packages/kernel-new/src/kernel_new.ml" ]
                ~actual:file_paths in
              Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
              Ok ()
        ))

let test_check_explicit_path_handles_hyphenated_package_names = fun _ctx ->
  with_tempdir_result "riot_check_explicit_path_hyphenated"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let kernel_root = Path.(workspace_root / Path.v "packages/kernel") in
      let kernel_new_root = Path.(workspace_root / Path.v "packages/kernel-new") in
      let kernel_source = Path.(kernel_root / Path.v "src/kernel.ml") in
      let kernel_new_source = Path.(kernel_new_root / Path.v "src/kernel_new.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package ~name:"kernel" ~path:kernel_root ~relative_path:(Path.v "packages/kernel") ();
          make_package
            ~name:"kernel-new"
            ~path:kernel_new_root
            ~relative_path:(Path.v "packages/kernel-new")
            ();
        ] in
      match write_file kernel_source "let old_runtime = 1\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file kernel_new_source "let new_runtime = 2\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check [ "check"; "--json"; "packages/kernel-new/src" ]
              |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"explicit kernel-new path should select kernel-new";
              let events = parse_jsonl (stdout_contents ()) in
              let package_events =
                events
                |> List.filter_map
                  (fun json ->
                    match Data.Json.get_field "type" json, Data.Json.get_field "package_name" json with
                    | Some (Data.Json.String "check_package_planning_start"), Some (Data.Json.String package_name) -> Some package_name
                    | _ -> None)
              in
              let file_paths =
                events
                |> List.filter_map
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_file") -> (
                        match Data.Json.get_field "result" json with
                        | Some result_json -> Data.Json.get_field "path" result_json
                        | None -> None
                      )
                    | _ -> None)
              in
              let () = Test.assert_equal ~expected:[ "kernel-new" ] ~actual:package_events in
              let () = Test.assert_equal
                ~expected:[ Data.Json.String "packages/kernel-new/src/kernel_new.ml" ]
                ~actual:file_paths in
              Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
              Ok ()
        ))

let test_check_explicit_file_handles_hyphenated_package_names = fun _ctx ->
  with_tempdir_result "riot_check_explicit_file_hyphenated"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let kernel_root = Path.(workspace_root / Path.v "packages/kernel") in
      let kernel_new_root = Path.(workspace_root / Path.v "packages/kernel-new") in
      let kernel_source = Path.(kernel_root / Path.v "src/kernel.ml") in
      let kernel_new_source = Path.(kernel_new_root / Path.v "src/kernel_new.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package ~name:"kernel" ~path:kernel_root ~relative_path:(Path.v "packages/kernel") ();
          make_package
            ~name:"kernel-new"
            ~path:kernel_new_root
            ~relative_path:(Path.v "packages/kernel-new")
            ();
        ] in
      match write_file kernel_source "let old_runtime = 1\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file kernel_new_source "let new_runtime = 2\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check
                [ "check"; "--json"; "packages/kernel-new/src/kernel_new.ml" ]
              |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"explicit kernel-new file should select kernel-new";
              let events = parse_jsonl (stdout_contents ()) in
              let package_events =
                events
                |> List.filter_map
                  (fun json ->
                    match Data.Json.get_field "type" json, Data.Json.get_field "package_name" json with
                    | Some (Data.Json.String "check_package_planning_start"), Some (Data.Json.String package_name) -> Some package_name
                    | _ -> None)
              in
              let file_paths =
                events
                |> List.filter_map
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_file") -> (
                        match Data.Json.get_field "result" json with
                        | Some result_json -> Data.Json.get_field "path" result_json
                        | None -> None
                      )
                    | _ -> None)
              in
              let () = Test.assert_equal ~expected:[ "kernel-new" ] ~actual:package_events in
              let () = Test.assert_equal
                ~expected:[ Data.Json.String "packages/kernel-new/src/kernel_new.ml" ]
                ~actual:file_paths in
              Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
              Ok ()
        ))

let test_check_package_filter_handles_hyphenated_package_names_after_workspace_prepare = fun _ctx ->
  with_tempdir_result "riot_check_package_filter_hyphenated_prepared"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let kernel_root = Path.(workspace_root / Path.v "packages/kernel") in
      let kernel_new_root = Path.(workspace_root / Path.v "packages/kernel-new") in
      let kernel_source = Path.(kernel_root / Path.v "src/kernel.ml") in
      let kernel_new_source = Path.(kernel_new_root / Path.v "src/kernel_new.ml") in
      match
        write_file Path.(workspace_root / Path.v "riot.toml")
          {ocaml|
[workspace]
members = [
  "packages/kernel",
  "packages/kernel-new",
]
|ocaml}
      with
      | Error err -> Error err
      | Ok () -> (
          match
            write_file Path.(kernel_root / Path.v "riot.toml")
              {ocaml|
[package]
name = "kernel"
version = "0.0.1"

[lib]
path = "src/kernel.ml"
|ocaml}
          with
          | Error err -> Error err
          | Ok () -> (
              match
                write_file Path.(kernel_new_root / Path.v "riot.toml")
                  {ocaml|
[package]
name = "kernel-new"
version = "0.0.1"

[lib]
path = "src/kernel_new.ml"
|ocaml}
              with
              | Error err -> Error err
              | Ok () -> (
                  match write_file kernel_source "let old_runtime = 1\n" with
                  | Error err -> Error err
                  | Ok () -> (
                      match write_file kernel_new_source "let new_runtime = 2\n" with
                      | Error err -> Error err
                      | Ok () -> (
                          match scan_workspace_result workspace_root with
                          | Error err -> Error err
                          | Ok workspace ->
                              let matches = parse_check [ "check"; "--json"; "-p"; "kernel-new" ]
                              |> Result.expect ~msg:"parse check args" in
                              let events = ref [] in
                              Riot_check.run
                                ~workspace
                                ~on_event:(fun event -> events := event :: !events)
                                matches
                              |> Result.expect ~msg:"prepared workspace check should select kernel-new";
                              let planning_packages =
                                !events
                                |> List.rev
                                |> List.filter_map
                                  (
                                    function
                                    | Riot_check.Check.Event.PackagePlanningStarted {
                                      package_name;
                                      _
                                    } -> Some package_name
                                    | _ -> None
                                  )
                              in
                              let checked_paths =
                                !events
                                |> List.rev
                                |> List.filter_map
                                  (
                                    function
                                    | Riot_check.Check.Event.File checked_file -> Some (Riot_check.Check.State.checked_file_path
                                      checked_file
                                    |> Path.normalize
                                    |> Path.strip_prefix ~prefix:(Path.normalize workspace_root)
                                    |> Result.map Path.to_string
                                    |> Result.unwrap_or
                                      ~default:(Riot_check.Check.State.checked_file_path checked_file
                                      |> Path.to_string))
                                    | _ -> None
                                  )
                              in
                              if not (planning_packages = [ "kernel-new" ]) then
                                Error ("expected prepared workspace planning packages to be [kernel-new], got ["
                                ^ String.concat ", " planning_packages
                                ^ "]")
                              else if
                                not (checked_paths = [ "packages/kernel-new/src/kernel_new.ml" ])
                              then
                                Error ("expected prepared workspace checked paths to be [packages/kernel-new/src/kernel_new.ml], got ["
                                ^ String.concat ", " checked_paths
                                ^ "]")
                              else
                                Ok ()
                        )
                    )
                )
            )
        ))

let test_check_package_filter_uses_package_session_for_cross_file_exports = fun _ctx ->
  with_tempdir_result "riot_check_package_session"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let colors_root = Path.(workspace_root / Path.v "packages/colors") in
      let helper_source = Path.(colors_root / Path.v "src/helper.ml") in
      let colors_source = Path.(colors_root / Path.v "src/colors.ml") in
      let demo_source = Path.(colors_root / Path.v "examples/blend_demo.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package ~name:"colors" ~path:colors_root ~relative_path:(Path.v "packages/colors") ()
        ] in
      match write_file helper_source "let twice x = x + x\n" with
      | Error err -> Error err
      | Ok () ->
          match write_file colors_source "let id x = Helper.twice x\n" with
          | Error err -> Error err
          | Ok () ->
              match write_file demo_source "open Colors\nlet answer = id 21\n" with
              | Error err -> Error err
              | Ok () ->
                  let matches = parse_check [ "check"; "--json"; "-p"; "colors" ]
                  |> Result.expect ~msg:"parse check args" in
                  let stdout, stdout_contents = make_capture_writer () in
                  let stderr, stderr_contents = make_capture_writer () in
                  Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should use sibling source exports";
                  let events = parse_jsonl (stdout_contents ()) in
                  let file_events =
                    events
                    |> List.filter
                      (fun json ->
                        match Data.Json.get_field "type" json with
                        | Some (Data.Json.String "check_file") -> true
                        | _ -> false)
                  in
                  let file_paths =
                    file_events
                    |> List.filter_map
                      (fun json ->
                        match Data.Json.get_field "result" json with
                        | Some result_json -> Data.Json.get_field "path" result_json
                        | None -> None)
                    |> List.sort compare
                  in
                  Test.assert_equal
                    ~expected:([
                      Data.Json.String "packages/colors/examples/blend_demo.ml";
                      Data.Json.String "packages/colors/src/colors.ml";
                      Data.Json.String "packages/colors/src/helper.ml";
                    ]
                    |> List.sort compare)
                    ~actual:file_paths;
                  let diagnostic_count =
                    events
                    |> List.filter
                      (fun json ->
                        match Data.Json.get_field "type" json with
                        | Some (Data.Json.String "check_diagnostic") -> true
                        | _ -> false)
                    |> List.length
                  in
                  Test.assert_equal ~expected:0 ~actual:diagnostic_count;
                  Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                  Ok ())

let test_check_package_filter_emits_authoritative_package_engine_event = fun _ctx ->
  with_tempdir_result "riot_check_package_session_events"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let colors_root = Path.(workspace_root / Path.v "packages/colors") in
      let helper_source = Path.(colors_root / Path.v "src/helper.ml") in
      let colors_source = Path.(colors_root / Path.v "src/colors.ml") in
      let demo_source = Path.(colors_root / Path.v "examples/blend_demo.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package ~name:"colors" ~path:colors_root ~relative_path:(Path.v "packages/colors") ()
        ] in
      match write_file helper_source "let twice x = x + x\n" with
      | Error err -> Error err
      | Ok () ->
          match write_file colors_source "let id x = Helper.twice x\n" with
          | Error err -> Error err
          | Ok () ->
              match write_file demo_source "open Colors\nlet answer = id 21\n" with
              | Error err -> Error err
              | Ok () ->
                  let matches = parse_check [ "check"; "--json"; "-p"; "colors" ]
                  |> Result.expect ~msg:"parse check args" in
                  let stdout, stdout_contents = make_capture_writer () in
                  let stderr, stderr_contents = make_capture_writer () in
                  Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should use the authoritative package engine";
                  let events = parse_jsonl (stdout_contents ()) in
                  let engine_event =
                    events
                    |> List.find_opt
                      (fun json ->
                        match Data.Json.get_field "type" json, Data.Json.get_field "package_name" json with
                        | Some (Data.Json.String "check_package_engine_selected"), Some (Data.Json.String "colors") -> true
                        | _ -> false)
                    |> Option.expect ~msg:"missing package engine event"
                  in
                  Test.assert_equal
                    ~expected:(Some (Data.Json.String "authoritative_package_engine"))
                    ~actual:(Data.Json.get_field "engine" engine_event);
                  Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                  Ok ())

let test_check_package_filter_does_not_emit_rooted_session_reconstruction_events = fun _ctx ->
  with_tempdir_result "riot_check_package_single_root_group"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let colors_root = Path.(workspace_root / Path.v "packages/colors") in
      let helper_source = Path.(colors_root / Path.v "src/helper.ml") in
      let colors_source = Path.(colors_root / Path.v "src/colors.ml") in
      let demo_source = Path.(colors_root / Path.v "examples/blend_demo.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package ~name:"colors" ~path:colors_root ~relative_path:(Path.v "packages/colors") ()
        ] in
      match write_file helper_source "let twice x = x + x\n" with
      | Error err -> Error err
      | Ok () ->
          match write_file colors_source "let id x = Helper.twice x\n" with
          | Error err -> Error err
          | Ok () ->
              match write_file demo_source "open Colors\nlet answer = id 21\n" with
              | Error err -> Error err
              | Ok () ->
                  let matches = parse_check [ "check"; "--json"; "-p"; "colors" ]
                  |> Result.expect ~msg:"parse check args" in
                  let stdout, stdout_contents = make_capture_writer () in
                  let stderr, stderr_contents = make_capture_writer () in
                  Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should avoid rooted-session reconstruction";
                  let events = parse_jsonl (stdout_contents ()) in
                  let event_types =
                    events
                    |> List.filter_map
                      (fun json ->
                        match Data.Json.get_field "type" json with
                        | Some (Data.Json.String event_type) -> Some event_type
                        | _ -> None)
                  in
                  let rooted_session_event_types = [
                    "check_package_session_seed_start";
                    "check_package_session_seed_finish";
                    "check_package_root_grouping_finish";
                    "check_package_snapshot_persistence_start";
                    "check_package_snapshot_persistence_finish";
                    "check_package_snapshot_checked_files_start";
                    "check_package_snapshot_checked_files_finish";
                    "check_package_snapshot_reload_start";
                    "check_package_snapshot_reload_finish";
                    "check_package_checked_group_assemble_start";
                    "check_package_checked_group_assemble_finish";
                  ]
                  in
                  let unexpected_rooted_events =
                    rooted_session_event_types
                    |> List.filter
                      (fun expected ->
                        List.exists (String.equal expected) event_types)
                  in
                  Test.assert_equal ~expected:[] ~actual:unexpected_rooted_events;
                  Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                  Ok ())

let test_check_package_filter_uses_package_session_for_cross_file_record_types = fun _ctx ->
  with_tempdir_result "riot_check_package_record_types"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let colors_root = Path.(workspace_root / Path.v "packages/colors") in
      let colors_source = Path.(colors_root / Path.v "src/colors.ml") in
      let demo_source = Path.(colors_root / Path.v "examples/blend_demo.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package ~name:"colors" ~path:colors_root ~relative_path:(Path.v "packages/colors") ()
        ] in
      match write_file colors_source "type point = { x: int; y: int }\n" with
      | Error err -> Error err
      | Ok () ->
          match write_file demo_source "open Colors\nlet origin = { x = 0; y = 0 }\nlet total point = point.x + point.y\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check [ "check"; "--json"; "-p"; "colors" ] |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should use sibling source record types";
              let events = parse_jsonl (stdout_contents ()) in
              let diagnostic_count =
                events
                |> List.filter
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_diagnostic") -> true
                    | _ -> false)
                |> List.length
              in
              Test.assert_equal ~expected:0 ~actual:diagnostic_count;
              Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
              Ok ())

let test_check_package_filter_loads_workspace_dependency_summaries = fun _ctx ->
  with_tempdir_result "riot_check_package_dependencies"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let std_root = Path.(workspace_root / Path.v "packages/std") in
      let tty_root = Path.(workspace_root / Path.v "packages/tty") in
      let std_source = Path.(std_root / Path.v "src/std.ml") in
      let tty_source = Path.(tty_root / Path.v "src/tty.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"std"
            ~path:std_root
            ~relative_path:(Path.v "packages/std")
            ~sources:{ empty_sources with src = [ Path.v "src/std.ml" ] }
            ();
          make_package
            ~name:"tty"
            ~path:tty_root
            ~relative_path:(Path.v "packages/tty")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{ empty_sources with src = [ Path.v "src/tty.ml" ] }
            ();
        ] in
      match write_file std_source "let twice x = x + x\n" with
      | Error err -> Error err
      | Ok () ->
          match write_file tty_source "let answer = Std.twice 21\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check [ "check"; "--json"; "-p"; "tty" ] |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should load workspace dependency summaries";
              let events = parse_jsonl (stdout_contents ()) in
              let file_paths =
                events
                |> List.filter_map
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_file") -> (
                        match Data.Json.get_field "result" json with
                        | Some result_json -> Data.Json.get_field "path" result_json
                        | None -> None
                      )
                    | _ -> None)
              in
              let diagnostic_count =
                events
                |> List.filter
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_diagnostic") -> true
                    | _ -> false)
                |> List.length
              in
              Test.assert_equal ~expected:[ Data.Json.String "packages/tty/src/tty.ml" ] ~actual:file_paths;
              Test.assert_equal ~expected:0 ~actual:diagnostic_count;
              Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
              Ok ())

let test_check_package_filter_emits_cached_dependency_package_events = fun _ctx ->
  with_tempdir_result "riot_check_package_events_json"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let std_root = Path.(workspace_root / Path.v "packages/std") in
      let tty_root = Path.(workspace_root / Path.v "packages/tty") in
      let std_source = Path.(std_root / Path.v "src/std.ml") in
      let tty_source = Path.(tty_root / Path.v "src/tty.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"std"
            ~path:std_root
            ~relative_path:(Path.v "packages/std")
            ~sources:{ empty_sources with src = [ Path.v "src/std.ml" ] }
            ();
          make_package
            ~name:"tty"
            ~path:tty_root
            ~relative_path:(Path.v "packages/tty")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{ empty_sources with src = [ Path.v "src/tty.ml" ] }
            ();
        ] in
      match write_file std_source "let twice x = x + x\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file tty_source "let answer = Std.twice 21\n" with
          | Error err -> Error err
          | Ok () ->
              let prime_matches = parse_check [ "check"; "--json"; "-p"; "tty" ]
              |> Result.expect ~msg:"parse priming check args" in
              let prime_stdout, _prime_stdout_contents = make_capture_writer () in
              let prime_stderr, _prime_stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout:prime_stdout ~stderr:prime_stderr prime_matches
              |> Result.expect ~msg:"priming check should persist dependency typings";
              let matches = parse_check [ "check"; "--json"; "-p"; "tty" ] |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should succeed";
              let events = parse_jsonl (stdout_contents ()) in
              let package_events =
                events
                |> List.filter_map
                  (fun json ->
                    let package_name =
                      match Data.Json.get_field "package_name" json with
                      | Some (Data.Json.String package_name) -> Some package_name
                      | _ -> None
                    in
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_package") -> package_name
                    |> Option.map (fun name -> ("check_package", name))
                    | Some (Data.Json.String "check_package_cached") -> package_name
                    |> Option.map (fun name -> ("check_package_cached", name))
                    | _ -> None)
              in
              Test.assert_equal
                ~expected:[ ("check_package_cached", "std"); ("check_package", "tty") ]
                ~actual:package_events;
              Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
              Ok ()
        ))

let test_check_package_filter_human_output_shows_cached_dependency_progress = fun _ctx ->
  with_tempdir_result "riot_check_package_events_human"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let std_root = Path.(workspace_root / Path.v "packages/std") in
      let tty_root = Path.(workspace_root / Path.v "packages/tty") in
      let std_source = Path.(std_root / Path.v "src/std.ml") in
      let tty_source = Path.(tty_root / Path.v "src/tty.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"std"
            ~path:std_root
            ~relative_path:(Path.v "packages/std")
            ~sources:{ empty_sources with src = [ Path.v "src/std.ml" ] }
            ();
          make_package
            ~name:"tty"
            ~path:tty_root
            ~relative_path:(Path.v "packages/tty")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{ empty_sources with src = [ Path.v "src/tty.ml" ] }
            ();
        ] in
      match write_file std_source "let twice x = x + x\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file tty_source "let answer = Std.twice 21\n" with
          | Error err -> Error err
          | Ok () ->
              let prime_matches = parse_check [ "check"; "-p"; "tty" ] |> Result.expect ~msg:"parse priming check args" in
              let prime_stdout, _prime_stdout_contents = make_capture_writer () in
              let prime_stderr, _prime_stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout:prime_stdout ~stderr:prime_stderr prime_matches
              |> Result.expect ~msg:"priming check should persist dependency typings";
              let matches = parse_check [ "check"; "-p"; "tty" ] |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should succeed";
              Test.assert_equal ~expected:"" ~actual:(stdout_contents ());
              Test.assert_equal
                ~expected:(package_progress_line "Checking" "tty")
                ~actual:(stderr_contents ());
              Ok ()
        ))

let test_check_package_filter_loads_external_dependency_summaries = fun _ctx ->
  with_tempdir_result "riot_check_external_package_dependencies"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let external_root = Path.(tmpdir / Path.v "external/std") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      let external_source = Path.(external_root / Path.v "src/std.ml") in
      let app_source = Path.(app_root / Path.v "src/app.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"std"
            ~path:external_root
            ~relative_path:(Path.v "../external/std")
            ~sources:{ empty_sources with src = [ Path.v "src/std.ml" ] }
            ();
          make_package
            ~name:"app"
            ~path:app_root
            ~relative_path:(Path.v "packages/app")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{ empty_sources with src = [ Path.v "src/app.ml" ] }
            ();
        ] in
      match write_file external_source "let twice x = x + x\n" with
      | Error err -> Error err
      | Ok () ->
          match write_file app_source "let answer = Std.twice 21\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check [ "check"; "--json"; "-p"; "app" ] |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should load external dependency summaries";
              let events = parse_jsonl (stdout_contents ()) in
              let file_paths =
                events
                |> List.filter_map
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_file") -> (
                        match Data.Json.get_field "result" json with
                        | Some result_json -> Data.Json.get_field "path" result_json
                        | None -> None
                      )
                    | _ -> None)
              in
              let diagnostic_count =
                events
                |> List.filter
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_diagnostic") -> true
                    | _ -> false)
                |> List.length
              in
              Test.assert_equal ~expected:[ Data.Json.String "packages/app/src/app.ml" ] ~actual:file_paths;
              Test.assert_equal ~expected:0 ~actual:diagnostic_count;
              Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
              Ok ())

let test_check_package_filter_persists_module_typings_to_store = fun _ctx ->
  with_tempdir_result "riot_check_package_typ_store"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let std_root = Path.(workspace_root / Path.v "packages/std") in
      let tty_root = Path.(workspace_root / Path.v "packages/tty") in
      let std_source = Path.(std_root / Path.v "src/std.ml") in
      let tty_source = Path.(tty_root / Path.v "src/tty.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"std"
            ~path:std_root
            ~relative_path:(Path.v "packages/std")
            ~sources:{ empty_sources with src = [ Path.v "src/std.ml" ] }
            ();
          make_package
            ~name:"tty"
            ~path:tty_root
            ~relative_path:(Path.v "packages/tty")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{ empty_sources with src = [ Path.v "src/tty.ml" ] }
            ();
        ] in
      match write_file std_source "let twice x = x + x\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file tty_source "let answer = Std.twice 21\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check [ "check"; "--json"; "-p"; "tty" ] |> Result.expect ~msg:"parse check args" in
              let stdout, _stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should succeed and persist module typings";
              let typ_store = workspace_typ_store workspace in
              let std_typings = Typ.Store.load_module_typings typ_store ~module_name:"Std" in
              let tty_typings = Typ.Store.load_module_typings typ_store ~module_name:"Tty" in
              if not (Option.is_some std_typings) then
                Error "expected Std module typings to be persisted"
              else if not (Option.is_some tty_typings) then
                Error "expected Tty module typings to be persisted"
              else (
                Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                Ok ()
              )
        ))

let test_check_package_filter_persists_interface_shaped_module_typings = fun _ctx ->
  with_tempdir_result "riot_check_package_interface_typings"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let colors_root = Path.(workspace_root / Path.v "packages/colors") in
      let colors_impl = Path.(colors_root / Path.v "src/colors.ml") in
      let colors_intf = Path.(colors_root / Path.v "src/colors.mli") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"colors"
            ~path:colors_root
            ~relative_path:(Path.v "packages/colors")
            ~sources:{ empty_sources with src = [ Path.v "src/colors.ml"; Path.v "src/colors.mli" ] }
            ();
        ] in
      match write_file colors_impl "let answer = 42\nlet hidden = true\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file colors_intf "val answer : int\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check [ "check"; "--json"; "-p"; "colors" ] |> Result.expect ~msg:"parse check args" in
              let stdout, _stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should succeed and persist interface-shaped module typings";
              let typ_store = workspace_typ_store workspace in
              let colors_typings = Typ.Store.load_module_typings typ_store ~module_name:"Colors" in
              match colors_typings with
              | None -> Error "expected Colors module typings to be persisted"
              | Some typings ->
                  let exports = Typ.Model.ModuleTypings.exports typings
                  |> List.map (fun (path, _scheme) -> Typ.Model.SurfacePath.to_string path) in
                  Test.assert_equal ~expected:[ "answer" ] ~actual:exports;
                  Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                  Ok ()
        ))

let test_check_package_filter_reports_signature_inclusion_errors = fun _ctx ->
  with_tempdir_result "riot_check_package_signature_inclusion"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let colors_root = Path.(workspace_root / Path.v "packages/colors") in
      let colors_impl = Path.(colors_root / Path.v "src/colors.ml") in
      let colors_intf = Path.(colors_root / Path.v "src/colors.mli") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"colors"
            ~path:colors_root
            ~relative_path:(Path.v "packages/colors")
            ~sources:{ empty_sources with src = [ Path.v "src/colors.ml"; Path.v "src/colors.mli" ] }
            ();
        ] in
      match write_file colors_impl "let answer = true\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file colors_intf "val answer : int\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check [ "check"; "--json"; "-p"; "colors" ] |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              (
                match Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches with
                | Ok () -> Error "expected package check to fail on signature inclusion mismatch"
                | Error _ ->
                    let events = parse_jsonl (stdout_contents ()) in
                    let codes =
                      events
                      |> List.filter_map
                        (fun json ->
                          match Data.Json.get_field "type" json with
                          | Some (Data.Json.String "check_diagnostic") -> (
                              match Data.Json.get_field "diagnostic" json with
                              | Some diagnostic_json -> Data.Json.get_field "code" diagnostic_json
                              | None -> None
                            )
                          | _ -> None)
                    in
                    Test.assert_equal
                      ~expected:true
                      ~actual:(List.mem (Data.Json.String "TYP2011") codes);
                    Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                    Ok ()
              )
        ))

let test_check_match_coverage_warnings_do_not_fail = fun _ctx ->
  with_tempdir_result "riot_check_match_coverage_warnings"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let demo_root = Path.(workspace_root / Path.v "packages/demo") in
      let source_path = Path.(demo_root / Path.v "src/demo.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"demo"
            ~path:demo_root
            ~relative_path:(Path.v "packages/demo")
            ~sources:{ empty_sources with src = [ Path.v "src/demo.ml" ] }
            ();
        ] in
      match write_file source_path "let nonexhaustive x =\n  match x with\n  | Some value -> value\n\nlet redundant x =\n  match x with\n  | _ -> 0\n  | Some value -> value\n" with
      | Error err -> Error err
      | Ok () ->
          let matches = parse_check [ "check"; "--json"; "-p"; "demo" ] |> Result.expect ~msg:"parse check args" in
          let stdout, stdout_contents = make_capture_writer () in
          let stderr, stderr_contents = make_capture_writer () in
          Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should succeed when typ only emits match-coverage warnings";
          let events = parse_jsonl (stdout_contents ()) in
          let file_paths =
            events
            |> List.filter_map
              (fun json ->
                match Data.Json.get_field "type" json with
                | Some (Data.Json.String "check_file") -> (
                    match Data.Json.get_field "result" json with
                    | Some result_json -> Data.Json.get_field "path" result_json
                    | None -> None
                  )
                | _ -> None)
          in
          let diagnostics =
            events
            |> List.filter_map
              (fun json ->
                match Data.Json.get_field "type" json with
                | Some (Data.Json.String "check_diagnostic") -> (
                    match Data.Json.get_field "diagnostic" json with
                    | Some diagnostic_json -> (
                        match (
                          Data.Json.get_field "code" diagnostic_json,
                          Data.Json.get_field "severity" diagnostic_json,
                          Data.Json.get_field "message" diagnostic_json
                        ) with
                        | (Some (Data.Json.String code), Some (Data.Json.String severity), Some (Data.Json.String message)) -> Some (
                          code,
                          severity,
                          message
                        )
                        | _ -> None
                      )
                    | None -> None
                  )
                | _ -> None)
          in
          Test.assert_equal ~expected:[ Data.Json.String "packages/demo/src/demo.ml" ] ~actual:file_paths;
          Test.assert_equal
            ~expected:[
              ("TYP1012", "warning", "non-exhaustive match: missing case None");
              ("TYP1013", "warning", "match case is redundant");
            ]
            ~actual:diagnostics;
          Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
          Ok ())

let test_check_package_filter_reexports_workspace_dependency_summaries_via_include = fun _ctx ->
  with_tempdir_result "riot_check_package_dependency_include"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let std_root = Path.(workspace_root / Path.v "packages/std") in
      let tty_root = Path.(workspace_root / Path.v "packages/tty") in
      let std_source = Path.(std_root / Path.v "src/std.ml") in
      let tty_source = Path.(tty_root / Path.v "src/tty.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"std"
            ~path:std_root
            ~relative_path:(Path.v "packages/std")
            ~sources:{ empty_sources with src = [ Path.v "src/std.ml" ] }
            ();
          make_package
            ~name:"tty"
            ~path:tty_root
            ~relative_path:(Path.v "packages/tty")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{ empty_sources with src = [ Path.v "src/tty.ml" ] }
            ();
        ] in
      match write_file std_source "let twice x = x + x\n" with
      | Error err -> Error err
      | Ok () ->
          match write_file tty_source "include Std\nlet answer = twice 21\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check [ "check"; "--json"; "-p"; "tty" ] |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should reexport dependency summaries through include";
              let events = parse_jsonl (stdout_contents ()) in
              let file_paths =
                events
                |> List.filter_map
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_file") -> (
                        match Data.Json.get_field "result" json with
                        | Some result_json -> Data.Json.get_field "path" result_json
                        | None -> None
                      )
                    | _ -> None)
              in
              let diagnostic_count =
                events
                |> List.filter
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_diagnostic") -> true
                    | _ -> false)
                |> List.length
              in
              Test.assert_equal ~expected:[ Data.Json.String "packages/tty/src/tty.ml" ] ~actual:file_paths;
              Test.assert_equal ~expected:0 ~actual:diagnostic_count;
              Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
              Ok ())

let test_check_package_filter_persists_locally_built_dependency_modules = fun _ctx ->
  with_tempdir_result "riot_check_dependency_library_module_only"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let std_root = Path.(workspace_root / Path.v "packages/std") in
      let colors_root = Path.(workspace_root / Path.v "packages/colors") in
      let std_source = Path.(std_root / Path.v "src/std.ml") in
      let std_internal = Path.(std_root / Path.v "src/internal.ml") in
      let colors_source = Path.(colors_root / Path.v "src/colors.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"std"
            ~path:std_root
            ~relative_path:(Path.v "packages/std")
            ~library:Riot_model.Package.{ path = std_source }
            ~sources:{ empty_sources with src = [ Path.v "src/std.ml"; Path.v "src/internal.ml" ] }
            ();
          make_package
            ~name:"colors"
            ~path:colors_root
            ~relative_path:(Path.v "packages/colors")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{ empty_sources with src = [ Path.v "src/colors.ml" ] }
            ();
        ] in
      match write_file std_source "let twice x = x + x\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file std_internal "let broken = missing_value\n" with
          | Error err -> Error err
          | Ok () -> (
              match write_file colors_source "let answer = Std.twice 21\n" with
              | Error err -> Error err
              | Ok () ->
                  let matches = parse_check [ "check"; "--json"; "-p"; "colors" ]
                  |> Result.expect ~msg:"parse check args" in
                  let stdout, stdout_contents = make_capture_writer () in
                  let stderr, stderr_contents = make_capture_writer () in
                  Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should persist locally built dependency module typings";
                  let typ_store = workspace_typ_store workspace in
                  let std_typings = Typ.Store.load_module_typings typ_store ~module_name:"Std" in
                  let internal_typings = Typ.Store.load_module_typings typ_store ~module_name:"Internal" in
                  let events = parse_jsonl (stdout_contents ()) in
                  let diagnostic_count =
                    events
                    |> List.filter
                      (fun json ->
                        match Data.Json.get_field "type" json with
                        | Some (Data.Json.String "check_diagnostic") -> true
                        | _ -> false)
                    |> List.length
                  in
                  if not (Option.is_some std_typings) then
                    Error "expected dependency library module typings to be persisted"
                  else if not (Option.is_some internal_typings) then
                    Error "expected dependency package module typings to be cached locally"
                  else (
                    Test.assert_equal ~expected:0 ~actual:diagnostic_count;
                    Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                    Ok ()
                  )
            )
        ))

let test_check_package_filter_loads_dependency_library_reexports_from_sibling_sources = fun _ctx ->
  with_tempdir_result "riot_check_dependency_library_reexports"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let std_root = Path.(workspace_root / Path.v "packages/std") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      let std_source = Path.(std_root / Path.v "src/std.ml") in
      let calendar_source = Path.(std_root / Path.v "src/calendar.ml") in
      let app_source = Path.(app_root / Path.v "src/app.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"std"
            ~path:std_root
            ~relative_path:(Path.v "packages/std")
            ~library:Riot_model.Package.{ path = std_source }
            ~sources:{ empty_sources with src = [ Path.v "src/std.ml"; Path.v "src/calendar.ml" ] }
            ();
          make_package
            ~name:"app"
            ~path:app_root
            ~relative_path:(Path.v "packages/app")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{ empty_sources with src = [ Path.v "src/app.ml" ] }
            ();
        ] in
      match write_file std_source "module Calendar = Calendar\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file calendar_source "let epoch = 1970\n" with
          | Error err -> Error err
          | Ok () -> (
              match write_file app_source "open Std\nlet answer = Calendar.epoch\n" with
              | Error err -> Error err
              | Ok () ->
                  let matches = parse_check [ "check"; "--json"; "-p"; "app" ]
                  |> Result.expect ~msg:"parse check args" in
                  let stdout, stdout_contents = make_capture_writer () in
                  let stderr, stderr_contents = make_capture_writer () in
                  Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should load dependency library reexports from sibling sources";
                  let events = parse_jsonl (stdout_contents ()) in
                  let diagnostic_count =
                    events
                    |> List.filter
                      (fun json ->
                        match Data.Json.get_field "type" json with
                        | Some (Data.Json.String "check_diagnostic") -> true
                        | _ -> false)
                    |> List.length
                  in
                  Test.assert_equal ~expected:0 ~actual:diagnostic_count;
                  Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                  Ok ()
            )
        ))

let test_check_package_filter_uses_sibling_reexported_dependency_record_types = fun _ctx ->
  with_tempdir_result "riot_check_package_dependency_record_reexport"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let support_root = Path.(workspace_root / Path.v "packages/support") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      let support_source = Path.(support_root / Path.v "src/support.ml") in
      let proxy_source = Path.(app_root / Path.v "src/proxy.ml") in
      let app_source = Path.(app_root / Path.v "src/app.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"support"
            ~path:support_root
            ~relative_path:(Path.v "packages/support")
            ~sources:{ empty_sources with src = [ Path.v "src/support.ml" ] }
            ();
          make_package
            ~name:"app"
            ~path:app_root
            ~relative_path:(Path.v "packages/app")
            ~dependencies:[ make_dependency "support" ]
            ~sources:{ empty_sources with src = [ Path.v "src/app.ml"; Path.v "src/proxy.ml" ] }
            ();
        ] in
      match write_file support_source "type point = { x: int; y: int }\n" with
      | Error err -> Error err
      | Ok () ->
          match write_file proxy_source "include Support\nlet origin = { x = 0; y = 0 }\n" with
          | Error err -> Error err
          | Ok () ->
              match write_file app_source "let total = Proxy.origin.x + Proxy.origin.y\n" with
              | Error err -> Error err
              | Ok () ->
                  let matches = parse_check [ "check"; "--json"; "-p"; "app" ]
                  |> Result.expect ~msg:"parse check args" in
                  let stdout, stdout_contents = make_capture_writer () in
                  let stderr, stderr_contents = make_capture_writer () in
                  Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should use sibling dependency record reexports";
                  let events = parse_jsonl (stdout_contents ()) in
                  let diagnostic_count =
                    events
                    |> List.filter
                      (fun json ->
                        match Data.Json.get_field "type" json with
                        | Some (Data.Json.String "check_diagnostic") -> true
                        | _ -> false)
                    |> List.length
                  in
                  Test.assert_equal ~expected:0 ~actual:diagnostic_count;
                  Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                  Ok ())

let test_check_package_filter_reexports_same_named_workspace_modules_via_alias = fun _ctx ->
  with_tempdir_result "riot_check_package_same_named_module_alias"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let tty_root = Path.(workspace_root / Path.v "packages/tty") in
      let color_source = Path.(tty_root / Path.v "src/color.ml") in
      let style_source = Path.(tty_root / Path.v "src/style.ml") in
      let tty_source = Path.(tty_root / Path.v "src/tty.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"tty"
            ~path:tty_root
            ~relative_path:(Path.v "packages/tty")
            ~sources:{
              empty_sources
              with src = [ Path.v "src/color.ml"; Path.v "src/style.ml"; Path.v "src/tty.ml" ]
            }
            ();
        ] in
      match write_file color_source "let make value = value\nlet shade value = value + 1\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file style_source "let bold value = value\n" with
          | Error err -> Error err
          | Ok () -> (
              match write_file tty_source "module Color = Color\nmodule Style = Style\nlet answer = Style.bold (Color.shade (Color.make 1))\n" with
              | Error err -> Error err
              | Ok () ->
                  let matches = parse_check [ "check"; "--json"; "-p"; "tty" ]
                  |> Result.expect ~msg:"parse check args" in
                  let stdout, stdout_contents = make_capture_writer () in
                  let stderr, stderr_contents = make_capture_writer () in
                  Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should reexport same-named workspace module summaries through aliases";
                  let events = parse_jsonl (stdout_contents ()) in
                  let file_paths =
                    events
                    |> List.filter_map
                      (fun json ->
                        match Data.Json.get_field "type" json with
                        | Some (Data.Json.String "check_file") -> (
                            match Data.Json.get_field "result" json with
                            | Some result_json -> Data.Json.get_field "path" result_json
                            | None -> None
                          )
                        | _ -> None)
                  in
                  let diagnostic_count =
                    events
                    |> List.filter
                      (fun json ->
                        match Data.Json.get_field "type" json with
                        | Some (Data.Json.String "check_diagnostic") -> true
                        | _ -> false)
                    |> List.length
                  in
                  Test.assert_equal
                    ~expected:[
                      Data.Json.String "packages/tty/src/color.ml";
                      Data.Json.String "packages/tty/src/style.ml";
                      Data.Json.String "packages/tty/src/tty.ml";
                    ]
                    ~actual:file_paths;
                  Test.assert_equal ~expected:0 ~actual:diagnostic_count;
                  Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                  Ok ()
            )
        ))

let test_check_expansive_bindings_stay_monomorphic = fun _ctx ->
  with_tempdir_result "riot_check_value_restriction"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let demo_root = Path.(workspace_root / Path.v "packages/demo") in
      let source_path = Path.(demo_root / Path.v "src/demo.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"demo"
            ~path:demo_root
            ~relative_path:(Path.v "packages/demo")
            ~sources:{ empty_sources with src = [ Path.v "src/demo.ml" ] }
            ();
        ] in
      match write_file source_path "let id x = x\nlet alias = id id\nlet _ = alias 1\nlet _ = alias true\n" with
      | Error err -> Error err
      | Ok () ->
          let matches = parse_check [ "check"; "--json"; "-p"; "demo" ] |> Result.expect ~msg:"parse check args" in
          let stdout, stdout_contents = make_capture_writer () in
          let stderr, stderr_contents = make_capture_writer () in
          (
            match Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches with
            | Ok () -> Error "expected package check to fail for expansive value-restriction violation"
            | Error _ ->
                let events = parse_jsonl (stdout_contents ()) in
                let diagnostic_messages =
                  events
                  |> List.filter_map
                    (fun json ->
                      match Data.Json.get_field "type" json with
                      | Some (Data.Json.String "check_diagnostic") -> (
                          match Data.Json.get_field "diagnostic" json with
                          | Some diagnostic_json -> (
                              match Data.Json.get_field "message" diagnostic_json with
                              | Some (Data.Json.String message) -> Some message
                              | _ -> None
                            )
                          | None -> None
                        )
                      | _ -> None)
                in
                Test.assert_equal ~expected:[ "type mismatch: expected int but got bool" ] ~actual:diagnostic_messages;
                Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                Ok ()
          ))

let test_check_package_filter_preserves_nested_same_named_alias_reexports = fun _ctx ->
  with_tempdir_result "riot_check_package_nested_same_named_module_alias"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let std_root = Path.(workspace_root / Path.v "packages/std") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      let cell_source = Path.(std_root / Path.v "src/cell.ml") in
      let sync_source = Path.(std_root / Path.v "src/sync.ml") in
      let std_source = Path.(std_root / Path.v "src/std.ml") in
      let app_source = Path.(app_root / Path.v "src/app.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"std"
            ~path:std_root
            ~relative_path:(Path.v "packages/std")
            ~sources:{
              empty_sources
              with src = [ Path.v "src/cell.ml"; Path.v "src/sync.ml"; Path.v "src/std.ml" ]
            }
            ();
          make_package
            ~name:"app"
            ~path:app_root
            ~relative_path:(Path.v "packages/app")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{ empty_sources with src = [ Path.v "src/app.ml" ] }
            ();
        ] in
      match write_file cell_source "let create value = value\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file sync_source "module Cell = Cell\n" with
          | Error err -> Error err
          | Ok () -> (
              match write_file std_source "module Sync = Sync\n" with
              | Error err -> Error err
              | Ok () -> (
                  match write_file app_source "open Std.Sync\nlet answer = Cell.create 1\n" with
                  | Error err -> Error err
                  | Ok () ->
                      let matches = parse_check [ "check"; "--json"; "-p"; "app" ]
                      |> Result.expect ~msg:"parse check args" in
                      let stdout, stdout_contents = make_capture_writer () in
                      let stderr, stderr_contents = make_capture_writer () in
                      Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches
                      |> Result.expect ~msg:"package check should preserve nested same-named alias reexports";
                      let events = parse_jsonl (stdout_contents ()) in
                      let file_paths =
                        events
                        |> List.filter_map
                          (fun json ->
                            match Data.Json.get_field "type" json with
                            | Some (Data.Json.String "check_file") -> (
                                match Data.Json.get_field "result" json with
                                | Some result_json -> Data.Json.get_field "path" result_json
                                | None -> None
                              )
                            | _ -> None)
                      in
                      let diagnostic_count =
                        events
                        |> List.filter
                          (fun json ->
                            match Data.Json.get_field "type" json with
                            | Some (Data.Json.String "check_diagnostic") -> true
                            | _ -> false)
                        |> List.length
                      in
                      Test.assert_equal
                        ~expected:[ Data.Json.String "packages/app/src/app.ml" ]
                        ~actual:file_paths;
                      Test.assert_equal ~expected:0 ~actual:diagnostic_count;
                      Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                      Ok ()
                )
            )
        ))

let test_check_package_filter_preserves_directory_same_named_alias_reexports = fun _ctx ->
  with_tempdir_result "riot_check_package_directory_same_named_module_alias"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let kernel_root = Path.(workspace_root / Path.v "packages/kernel") in
      let std_root = Path.(workspace_root / Path.v "packages/std") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      let kernel_cell_source = Path.(kernel_root / Path.v "src/sync/cell.mli") in
      let kernel_sync_source = Path.(kernel_root / Path.v "src/sync/sync.mli") in
      let kernel_source = Path.(kernel_root / Path.v "src/kernel.mli") in
      let std_sync_source = Path.(std_root / Path.v "src/sync.mli") in
      let std_source = Path.(std_root / Path.v "src/std.mli") in
      let app_source = Path.(app_root / Path.v "src/app.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"kernel"
            ~path:kernel_root
            ~relative_path:(Path.v "packages/kernel")
            ~sources:{
              empty_sources
              with src = [
                Path.v "src/sync/cell.mli";
                Path.v "src/sync/sync.mli";
                Path.v "src/kernel.mli";
              ]
            }
            ();
          make_package
            ~name:"std"
            ~path:std_root
            ~relative_path:(Path.v "packages/std")
            ~dependencies:[ make_dependency "kernel" ]
            ~sources:{ empty_sources with src = [ Path.v "src/sync.mli"; Path.v "src/std.mli" ] }
            ();
          make_package
            ~name:"app"
            ~path:app_root
            ~relative_path:(Path.v "packages/app")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{ empty_sources with src = [ Path.v "src/app.ml" ] }
            ();
        ] in
      match write_file kernel_cell_source "val create : 'a -> 'a\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file kernel_sync_source "module Cell = Cell\n" with
          | Error err -> Error err
          | Ok () -> (
              match write_file kernel_source "module Sync = Sync\n" with
              | Error err -> Error err
              | Ok () -> (
                  match write_file std_sync_source "include module type of Kernel.Sync\n" with
                  | Error err -> Error err
                  | Ok () -> (
                      match write_file std_source "module Sync = Sync\n" with
                      | Error err -> Error err
                      | Ok () -> (
                          match write_file app_source "open Std.Sync\nlet answer = Cell.create 1\n" with
                          | Error err -> Error err
                          | Ok () ->
                              let matches = parse_check [ "check"; "--json"; "-p"; "app" ]
                              |> Result.expect ~msg:"parse check args" in
                              let stdout, stdout_contents = make_capture_writer () in
                              let stderr, stderr_contents = make_capture_writer () in
                              match Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches with
                              | Error exn -> Error ("package check should preserve directory same-named alias reexports: "
                              ^ Exception.to_string exn
                              ^ "\nstdout:\n"
                              ^ stdout_contents ()
                              ^ "\nstderr:\n"
                              ^ stderr_contents ())
                              | Ok () ->
                                  let events = parse_jsonl (stdout_contents ()) in
                                  let file_paths =
                                    events
                                    |> List.filter_map
                                      (fun json ->
                                        match Data.Json.get_field "type" json with
                                        | Some (Data.Json.String "check_file") -> (
                                            match Data.Json.get_field "result" json with
                                            | Some result_json -> Data.Json.get_field "path" result_json
                                            | None -> None
                                          )
                                        | _ -> None)
                                  in
                                  let diagnostic_count =
                                    events
                                    |> List.filter
                                      (fun json ->
                                        match Data.Json.get_field "type" json with
                                        | Some (Data.Json.String "check_diagnostic") -> true
                                        | _ -> false)
                                    |> List.length
                                  in
                                  Test.assert_equal
                                    ~expected:[ Data.Json.String "packages/app/src/app.ml" ]
                                    ~actual:file_paths;
                                  Test.assert_equal ~expected:0 ~actual:diagnostic_count;
                                  Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                                  Ok ()
                        )
                    )
                )
            )
        ))

let test_check_relaxed_value_restriction_preserves_covariant_lists = fun _ctx ->
  with_tempdir_result "riot_check_relaxed_value_restriction"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let demo_root = Path.(workspace_root / Path.v "packages/demo") in
      let source_path = Path.(demo_root / Path.v "src/demo.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"demo"
            ~path:demo_root
            ~relative_path:(Path.v "packages/demo")
            ~sources:{ empty_sources with src = [ Path.v "src/demo.ml" ] }
            ();
        ] in
      match write_file source_path "let make _ = []\nlet xs = make ()\nlet _ = 1 :: xs\nlet _ = true :: xs\n" with
      | Error err -> Error err
      | Ok () ->
          let matches = parse_check [ "check"; "--json"; "-p"; "demo" ] |> Result.expect ~msg:"parse check args" in
          let stdout, stdout_contents = make_capture_writer () in
          let stderr, stderr_contents = make_capture_writer () in
          Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should keep covariant expansive list bindings polymorphic";
          let events = parse_jsonl (stdout_contents ()) in
          let diagnostic_count =
            events
            |> List.filter
              (fun json ->
                match Data.Json.get_field "type" json with
                | Some (Data.Json.String "check_diagnostic") -> true
                | _ -> false)
            |> List.length
          in
          Test.assert_equal ~expected:0 ~actual:diagnostic_count;
          Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
          Ok ())

let test_check_relaxed_value_restriction_preserves_covariant_nominal_types = fun _ctx ->
  with_tempdir_result "riot_check_relaxed_value_restriction_nominal"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let demo_root = Path.(workspace_root / Path.v "packages/demo") in
      let source_path = Path.(demo_root / Path.v "src/demo.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"demo"
            ~path:demo_root
            ~relative_path:(Path.v "packages/demo")
            ~sources:{ empty_sources with src = [ Path.v "src/demo.ml" ] }
            ();
        ] in
      match write_file source_path "type 'a box = Box of 'a list\nlet make _ = Box []\nlet boxed = make ()\nlet _ = match boxed with Box xs -> 1 :: xs\nlet _ = match boxed with Box xs -> true :: xs\n" with
      | Error err -> Error err
      | Ok () ->
          let matches = parse_check [ "check"; "--json"; "-p"; "demo" ] |> Result.expect ~msg:"parse check args" in
          let stdout, stdout_contents = make_capture_writer () in
          let stderr, stderr_contents = make_capture_writer () in
          Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should keep covariant expansive nominal bindings polymorphic";
          let events = parse_jsonl (stdout_contents ()) in
          let diagnostic_count =
            events
            |> List.filter
              (fun json ->
                match Data.Json.get_field "type" json with
                | Some (Data.Json.String "check_diagnostic") -> true
                | _ -> false)
            |> List.length
          in
          Test.assert_equal ~expected:0 ~actual:diagnostic_count;
          Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
          Ok ())

let test_check_explicit_workspace_file_uses_sibling_source_exports = fun _ctx ->
  with_tempdir_result "riot_check_explicit_file_package_session"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let colors_root = Path.(workspace_root / Path.v "packages/colors") in
      let helper_source = Path.(colors_root / Path.v "src/helper.ml") in
      let colors_source = Path.(colors_root / Path.v "src/colors.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"colors"
            ~path:colors_root
            ~relative_path:(Path.v "packages/colors")
            ~sources:{ empty_sources with src = [ Path.v "src/helper.ml"; Path.v "src/colors.ml" ] }
            ();
        ] in
      match write_file helper_source "let twice x = x + x\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file colors_source "let answer = Helper.twice 21\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check [ "check"; "--json"; Path.to_string colors_source ]
              |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"explicit workspace file check should use sibling source exports";
              let events = parse_jsonl (stdout_contents ()) in
              let file_paths =
                events
                |> List.filter_map
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_file") -> (
                        match Data.Json.get_field "result" json with
                        | Some result_json -> Data.Json.get_field "path" result_json
                        | None -> None
                      )
                    | _ -> None)
              in
              let diagnostic_count =
                events
                |> List.filter
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_diagnostic") -> true
                    | _ -> false)
                |> List.length
              in
              Test.assert_equal
                ~expected:[ Data.Json.String "packages/colors/src/colors.ml" ]
                ~actual:file_paths;
              Test.assert_equal ~expected:0 ~actual:diagnostic_count;
              Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
              Ok ()
        ))

let test_check_explicit_relative_workspace_file_uses_sibling_source_exports = fun _ctx ->
  with_tempdir_result "riot_check_explicit_relative_file_package_session"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let colors_root = Path.(workspace_root / Path.v "packages/colors") in
      let helper_source = Path.(colors_root / Path.v "src/helper.ml") in
      let colors_source = Path.(colors_root / Path.v "src/colors.ml") in
      let relative_colors_source = Path.v "packages/colors/src/colors.ml" in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"colors"
            ~path:colors_root
            ~relative_path:(Path.v "packages/colors")
            ~sources:{ empty_sources with src = [ Path.v "src/helper.ml"; Path.v "src/colors.ml" ] }
            ();
        ] in
      match write_file helper_source "let twice x = x + x\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file colors_source "let answer = Helper.twice 21\n" with
          | Error err -> Error err
          | Ok () ->
              with_current_dir_result workspace_root
                (fun () ->
                  let matches =
                    match parse_check [ "check"; "--json"; Path.to_string relative_colors_source ] with
                    | Ok matches -> matches
                    | Error err -> panic ("parse check args: " ^ err)
                  in
                  let stdout, stdout_contents = make_capture_writer () in
                  let stderr, stderr_contents = make_capture_writer () in
                  match Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches with
                  | Error exn -> Error ("relative workspace file check failed: "
                  ^ Exception.to_string exn
                  ^ "\nstdout:\n"
                  ^ stdout_contents ()
                  ^ "\nstderr:\n"
                  ^ stderr_contents ())
                  | Ok () ->
                      let events = parse_jsonl (stdout_contents ()) in
                      let file_paths =
                        events
                        |> List.filter_map
                          (fun json ->
                            match Data.Json.get_field "type" json with
                            | Some (Data.Json.String "check_file") -> (
                                match Data.Json.get_field "result" json with
                                | Some result_json -> Data.Json.get_field "path" result_json
                                | None -> None
                              )
                            | _ -> None)
                      in
                      let diagnostic_count =
                        events
                        |> List.filter
                          (fun json ->
                            match Data.Json.get_field "type" json with
                            | Some (Data.Json.String "check_diagnostic") -> true
                            | _ -> false)
                        |> List.length
                      in
                      Test.assert_equal
                        ~expected:[ Data.Json.String "packages/colors/src/colors.ml" ]
                        ~actual:file_paths;
                      Test.assert_equal ~expected:0 ~actual:diagnostic_count;
                      Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                      Ok ())
        ))

let test_check_explicit_workspace_file_loads_dependency_summaries = fun _ctx ->
  with_tempdir_result "riot_check_explicit_file_dependencies"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let std_root = Path.(workspace_root / Path.v "packages/std") in
      let tty_root = Path.(workspace_root / Path.v "packages/tty") in
      let std_source = Path.(std_root / Path.v "src/std.ml") in
      let tty_source = Path.(tty_root / Path.v "src/tty.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"std"
            ~path:std_root
            ~relative_path:(Path.v "packages/std")
            ~sources:{ empty_sources with src = [ Path.v "src/std.ml" ] }
            ();
          make_package
            ~name:"tty"
            ~path:tty_root
            ~relative_path:(Path.v "packages/tty")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{ empty_sources with src = [ Path.v "src/tty.ml" ] }
            ();
        ] in
      match write_file std_source "let twice x = x + x\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file tty_source "let answer = Std.twice 21\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check [ "check"; "--json"; Path.to_string tty_source ]
              |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"explicit workspace file check should load dependency summaries";
              let events = parse_jsonl (stdout_contents ()) in
              let file_paths =
                events
                |> List.filter_map
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_file") -> (
                        match Data.Json.get_field "result" json with
                        | Some result_json -> Data.Json.get_field "path" result_json
                        | None -> None
                      )
                    | _ -> None)
              in
              let diagnostic_count =
                events
                |> List.filter
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_diagnostic") -> true
                    | _ -> false)
                |> List.length
              in
              Test.assert_equal ~expected:[ Data.Json.String "packages/tty/src/tty.ml" ] ~actual:file_paths;
              Test.assert_equal ~expected:0 ~actual:diagnostic_count;
              Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
              Ok ()
        ))

let test_check_package_filter_loads_transitive_workspace_dependency_summaries = fun _ctx ->
  with_tempdir_result "riot_check_package_transitive_dependencies"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let kernel_root = Path.(workspace_root / Path.v "packages/kernel") in
      let std_root = Path.(workspace_root / Path.v "packages/std") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      let kernel_env_source = Path.(kernel_root / Path.v "src/env.ml") in
      let kernel_source = Path.(kernel_root / Path.v "src/kernel.ml") in
      let std_source = Path.(std_root / Path.v "src/std.ml") in
      let app_source = Path.(app_root / Path.v "src/app.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"kernel"
            ~path:kernel_root
            ~relative_path:(Path.v "packages/kernel")
            ~sources:{ empty_sources with src = [ Path.v "src/env.ml"; Path.v "src/kernel.ml" ] }
            ();
          make_package
            ~name:"std"
            ~path:std_root
            ~relative_path:(Path.v "packages/std")
            ~dependencies:[ make_dependency "kernel" ]
            ~sources:{ empty_sources with src = [ Path.v "src/std.ml" ] }
            ();
          make_package
            ~name:"app"
            ~path:app_root
            ~relative_path:(Path.v "packages/app")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{ empty_sources with src = [ Path.v "src/app.ml" ] }
            ();
        ] in
      match write_file kernel_env_source "let getenv_exn name = name\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file kernel_source "module Env = Env\n" with
          | Error err -> Error err
          | Ok () -> (
              match write_file std_source "let sentinel = 21\n" with
              | Error err -> Error err
              | Ok () -> (
                  match write_file app_source "let answer = Kernel.Env.getenv_exn \"TERM\"\n" with
                  | Error err -> Error err
                  | Ok () ->
                      let matches = parse_check [ "check"; "--json"; "-p"; "app" ]
                      |> Result.expect ~msg:"parse check args" in
                      let stdout, stdout_contents = make_capture_writer () in
                      let stderr, stderr_contents = make_capture_writer () in
                      Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches
                      |> Result.expect ~msg:"package check should load transitive workspace dependency summaries";
                      let events = parse_jsonl (stdout_contents ()) in
                      let file_paths =
                        events
                        |> List.filter_map
                          (fun json ->
                            match Data.Json.get_field "type" json with
                            | Some (Data.Json.String "check_file") -> (
                                match Data.Json.get_field "result" json with
                                | Some result_json -> Data.Json.get_field "path" result_json
                                | None -> None
                              )
                            | _ -> None)
                      in
                      let diagnostic_count =
                        events
                        |> List.filter
                          (fun json ->
                            match Data.Json.get_field "type" json with
                            | Some (Data.Json.String "check_diagnostic") -> true
                            | _ -> false)
                        |> List.length
                      in
                      Test.assert_equal
                        ~expected:[ Data.Json.String "packages/app/src/app.ml" ]
                        ~actual:file_paths;
                      Test.assert_equal ~expected:0 ~actual:diagnostic_count;
                      Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                      Ok ()
                )
            )
        ))

let test_check_explicit_workspace_file_loads_transitive_dependency_summaries = fun _ctx ->
  with_tempdir_result "riot_check_explicit_file_transitive_dependencies"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let kernel_root = Path.(workspace_root / Path.v "packages/kernel") in
      let std_root = Path.(workspace_root / Path.v "packages/std") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      let kernel_env_source = Path.(kernel_root / Path.v "src/env.ml") in
      let kernel_source = Path.(kernel_root / Path.v "src/kernel.ml") in
      let std_source = Path.(std_root / Path.v "src/std.ml") in
      let app_source = Path.(app_root / Path.v "src/app.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"kernel"
            ~path:kernel_root
            ~relative_path:(Path.v "packages/kernel")
            ~sources:{ empty_sources with src = [ Path.v "src/env.ml"; Path.v "src/kernel.ml" ] }
            ();
          make_package
            ~name:"std"
            ~path:std_root
            ~relative_path:(Path.v "packages/std")
            ~dependencies:[ make_dependency "kernel" ]
            ~sources:{ empty_sources with src = [ Path.v "src/std.ml" ] }
            ();
          make_package
            ~name:"app"
            ~path:app_root
            ~relative_path:(Path.v "packages/app")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{ empty_sources with src = [ Path.v "src/app.ml" ] }
            ();
        ] in
      match write_file kernel_env_source "let getenv_exn name = name\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file kernel_source "module Env = Env\n" with
          | Error err -> Error err
          | Ok () -> (
              match write_file std_source "let sentinel = 21\n" with
              | Error err -> Error err
              | Ok () -> (
                  match write_file app_source "let answer = Kernel.Env.getenv_exn \"TERM\"\n" with
                  | Error err -> Error err
                  | Ok () ->
                      let matches = parse_check [ "check"; "--json"; Path.to_string app_source ]
                      |> Result.expect ~msg:"parse check args" in
                      let stdout, stdout_contents = make_capture_writer () in
                      let stderr, stderr_contents = make_capture_writer () in
                      Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches
                      |> Result.expect ~msg:"explicit workspace file check should load transitive dependency summaries";
                      let events = parse_jsonl (stdout_contents ()) in
                      let file_paths =
                        events
                        |> List.filter_map
                          (fun json ->
                            match Data.Json.get_field "type" json with
                            | Some (Data.Json.String "check_file") -> (
                                match Data.Json.get_field "result" json with
                                | Some result_json -> Data.Json.get_field "path" result_json
                                | None -> None
                              )
                            | _ -> None)
                      in
                      let diagnostic_count =
                        events
                        |> List.filter
                          (fun json ->
                            match Data.Json.get_field "type" json with
                            | Some (Data.Json.String "check_diagnostic") -> true
                            | _ -> false)
                        |> List.length
                      in
                      Test.assert_equal
                        ~expected:[ Data.Json.String "packages/app/src/app.ml" ]
                        ~actual:file_paths;
                      Test.assert_equal ~expected:0 ~actual:diagnostic_count;
                      Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                      Ok ()
                )
            )
        ))

let test_check_package_filter_keeps_dependency_summaries_when_dependency_has_broken_sources = fun _ctx ->
  with_tempdir_result "riot_check_dependency_summary_fallback"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let support_root = Path.(workspace_root / Path.v "packages/support") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      let support_source = Path.(support_root / Path.v "src/support.ml") in
      let support_broken_source = Path.(support_root / Path.v "src/broken.ml") in
      let app_source = Path.(app_root / Path.v "src/app.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"support"
            ~path:support_root
            ~relative_path:(Path.v "packages/support")
            ~sources:{ empty_sources with src = [ Path.v "src/support.ml"; Path.v "src/broken.ml" ] }
            ();
          make_package
            ~name:"app"
            ~path:app_root
            ~relative_path:(Path.v "packages/app")
            ~dependencies:[ make_dependency "support" ]
            ~sources:{ empty_sources with src = [ Path.v "src/app.ml" ] }
            ();
        ] in
      match write_file support_source "let answer = 42\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file support_broken_source "let broken = Missing.answer\n" with
          | Error err -> Error err
          | Ok () -> (
              match write_file app_source "let answer = Support.answer\n" with
              | Error err -> Error err
              | Ok () ->
                  let matches = parse_check [ "check"; "--json"; "-p"; "app" ]
                  |> Result.expect ~msg:"parse check args" in
                  let stdout, stdout_contents = make_capture_writer () in
                  let stderr, stderr_contents = make_capture_writer () in
                  match Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches with
                  | Error exn -> Error ("package check should keep dependency summaries from healthy roots: "
                  ^ Exception.to_string exn
                  ^ "\nstdout:\n"
                  ^ stdout_contents ()
                  ^ "\nstderr:\n"
                  ^ stderr_contents ())
                  | Ok () ->
                      let events = parse_jsonl (stdout_contents ()) in
                      let file_paths =
                        events
                        |> List.filter_map
                          (fun json ->
                            match Data.Json.get_field "type" json with
                            | Some (Data.Json.String "check_file") -> (
                                match Data.Json.get_field "result" json with
                                | Some result_json -> Data.Json.get_field "path" result_json
                                | None -> None
                              )
                            | _ -> None)
                      in
                      let diagnostic_count =
                        events
                        |> List.filter
                          (fun json ->
                            match Data.Json.get_field "type" json with
                            | Some (Data.Json.String "check_diagnostic") -> true
                            | _ -> false)
                        |> List.length
                      in
                      Test.assert_equal
                        ~expected:[ Data.Json.String "packages/app/src/app.ml" ]
                        ~actual:file_paths;
                      Test.assert_equal ~expected:0 ~actual:diagnostic_count;
                      Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                      Ok ()
            )
        ))

let test_check_package_filter_uses_dependency_sibling_interface_alias_exports = fun _ctx ->
  with_tempdir_result "riot_check_dependency_interface_alias_exports"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let std_root = Path.(workspace_root / Path.v "packages/std") in
      let colors_root = Path.(workspace_root / Path.v "packages/colors") in
      let float_source = Path.(std_root / Path.v "src/float.mli") in
      let std_source = Path.(std_root / Path.v "src/std.mli") in
      let colors_source = Path.(colors_root / Path.v "src/colors.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"std"
            ~path:std_root
            ~relative_path:(Path.v "packages/std")
            ~sources:{ empty_sources with src = [ Path.v "src/float.mli"; Path.v "src/std.mli" ] }
            ();
          make_package
            ~name:"colors"
            ~path:colors_root
            ~relative_path:(Path.v "packages/colors")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{ empty_sources with src = [ Path.v "src/colors.ml" ] }
            ();
        ] in
      match write_file float_source "include module type of Stdlib.Float\nval to_string : ?precision:int -> float -> string\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file std_source "module Float = Float\n" with
          | Error err -> Error err
          | Ok () -> (
              match write_file colors_source "open Std\nlet rendered = Float.to_string 1.0\n" with
              | Error err -> Error err
              | Ok () ->
                  let matches = parse_check [ "check"; "--json"; "-p"; "colors" ]
                  |> Result.expect ~msg:"parse check args" in
                  let stdout, stdout_contents = make_capture_writer () in
                  let stderr, stderr_contents = make_capture_writer () in
                  Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should use dependency sibling interface alias exports";
                  let events = parse_jsonl (stdout_contents ()) in
                  let file_paths =
                    events
                    |> List.filter_map
                      (fun json ->
                        match Data.Json.get_field "type" json with
                        | Some (Data.Json.String "check_file") -> (
                            match Data.Json.get_field "result" json with
                            | Some result_json -> Data.Json.get_field "path" result_json
                            | None -> None
                          )
                        | _ -> None)
                  in
                  let diagnostic_count =
                    events
                    |> List.filter
                      (fun json ->
                        match Data.Json.get_field "type" json with
                        | Some (Data.Json.String "check_diagnostic") -> true
                        | _ -> false)
                    |> List.length
                  in
                  Test.assert_equal
                    ~expected:[ Data.Json.String "packages/colors/src/colors.ml" ]
                    ~actual:file_paths;
                  Test.assert_equal ~expected:0 ~actual:diagnostic_count;
                  Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                  Ok ()
            )
        ))

let test_check_package_filter_preserves_dependency_nested_alias_exports = fun _ctx ->
  with_tempdir_result "riot_check_dependency_nested_alias_exports"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let kernel_root = Path.(workspace_root / Path.v "packages/kernel") in
      let std_root = Path.(workspace_root / Path.v "packages/std") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      let cell_source = Path.(kernel_root / Path.v "src/cell.mli") in
      let kernel_sync_source = Path.(kernel_root / Path.v "src/sync.mli") in
      let kernel_source = Path.(kernel_root / Path.v "src/kernel.mli") in
      let std_sync_source = Path.(std_root / Path.v "src/sync.mli") in
      let std_source = Path.(std_root / Path.v "src/std.mli") in
      let app_source = Path.(app_root / Path.v "src/app.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"kernel"
            ~path:kernel_root
            ~relative_path:(Path.v "packages/kernel")
            ~sources:{
              empty_sources
              with src = [ Path.v "src/cell.mli"; Path.v "src/sync.mli"; Path.v "src/kernel.mli"; ]
            }
            ();
          make_package
            ~name:"std"
            ~path:std_root
            ~relative_path:(Path.v "packages/std")
            ~dependencies:[ make_dependency "kernel" ]
            ~sources:{ empty_sources with src = [ Path.v "src/sync.mli"; Path.v "src/std.mli" ] }
            ();
          make_package
            ~name:"app"
            ~path:app_root
            ~relative_path:(Path.v "packages/app")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{ empty_sources with src = [ Path.v "src/app.ml" ] }
            ();
        ] in
      match write_file cell_source "val create : 'a -> 'a\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file kernel_sync_source "module Cell = Cell\n" with
          | Error err -> Error err
          | Ok () -> (
              match write_file kernel_source "module Sync = Sync\n" with
              | Error err -> Error err
              | Ok () -> (
                  match write_file std_sync_source "include module type of Kernel.Sync\n" with
                  | Error err -> Error err
                  | Ok () -> (
                      match write_file std_source "module Sync = Sync\n" with
                      | Error err -> Error err
                      | Ok () -> (
                          match write_file app_source "open Std.Sync\nlet answer = Cell.create 1\n" with
                          | Error err -> Error err
                          | Ok () ->
                              let matches = parse_check [ "check"; "--json"; "-p"; "app" ]
                              |> Result.expect ~msg:"parse check args" in
                              let stdout, stdout_contents = make_capture_writer () in
                              let stderr, stderr_contents = make_capture_writer () in
                              Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches
                              |> Result.expect ~msg:"package check should preserve dependency nested alias exports";
                              let events = parse_jsonl (stdout_contents ()) in
                              let file_paths =
                                events
                                |> List.filter_map
                                  (fun json ->
                                    match Data.Json.get_field "type" json with
                                    | Some (Data.Json.String "check_file") -> (
                                        match Data.Json.get_field "result" json with
                                        | Some result_json -> Data.Json.get_field "path" result_json
                                        | None -> None
                                      )
                                    | _ -> None)
                              in
                              let diagnostic_count =
                                events
                                |> List.filter
                                  (fun json ->
                                    match Data.Json.get_field "type" json with
                                    | Some (Data.Json.String "check_diagnostic") -> true
                                    | _ -> false)
                                |> List.length
                              in
                              Test.assert_equal
                                ~expected:[ Data.Json.String "packages/app/src/app.ml" ]
                                ~actual:file_paths;
                              Test.assert_equal ~expected:0 ~actual:diagnostic_count;
                              Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                              Ok ()
                        )
                    )
                )
            )
        ))

let test_check_package_filter_canonicalizes_dependency_result_alias_exports = fun _ctx ->
  with_tempdir_result "riot_check_dependency_result_alias_exports"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let actors_root = Path.(workspace_root / Path.v "packages/actors") in
      let kernel_root = Path.(workspace_root / Path.v "packages/kernel") in
      let std_root = Path.(workspace_root / Path.v "packages/std") in
      let colors_root = Path.(workspace_root / Path.v "packages/colors") in
      let actors_source = Path.(actors_root / Path.v "src/actors.mli") in
      let kernel_source = Path.(kernel_root / Path.v "src/kernel.mli") in
      let global_source = Path.(std_root / Path.v "src/global.mli") in
      let std_source = Path.(std_root / Path.v "src/std.mli") in
      let colors_source = Path.(colors_root / Path.v "src/colors.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"actors"
            ~path:actors_root
            ~relative_path:(Path.v "packages/actors")
            ~sources:{ empty_sources with src = [ Path.v "src/actors.mli" ] }
            ();
          make_package
            ~name:"kernel"
            ~path:kernel_root
            ~relative_path:(Path.v "packages/kernel")
            ~sources:{ empty_sources with src = [ Path.v "src/kernel.mli" ] }
            ();
          make_package
            ~name:"std"
            ~path:std_root
            ~relative_path:(Path.v "packages/std")
            ~dependencies:[ make_dependency "actors"; make_dependency "kernel" ]
            ~sources:{ empty_sources with src = [ Path.v "src/global.mli"; Path.v "src/std.mli" ] }
            ();
          make_package
            ~name:"colors"
            ~path:colors_root
            ~relative_path:(Path.v "packages/colors")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{ empty_sources with src = [ Path.v "src/colors.ml" ] }
            ();
        ] in
      match write_file actors_source "module Process: sig\n  type exit_reason = exn\nend\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file kernel_source "type ('ok, 'error) result = ('ok, 'error) Stdlib.result\n" with
          | Error err -> Error err
          | Ok () -> (
              match write_file global_source "val spawn : (unit -> (unit, Actors.Process.exit_reason) Kernel.result) -> int\n" with
              | Error err -> Error err
              | Ok () -> (
                  match write_file std_source "include module type of Global\n" with
                  | Error err -> Error err
                  | Ok () -> (
                      match write_file colors_source "let pid = Std.spawn (fun () -> Ok ())\n" with
                      | Error err -> Error err
                      | Ok () ->
                          let matches = parse_check [ "check"; "--json"; "-p"; "colors" ]
                          |> Result.expect ~msg:"parse check args" in
                          let stdout, stdout_contents = make_capture_writer () in
                          let stderr, stderr_contents = make_capture_writer () in
                          Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches
                          |> Result.expect ~msg:"package check should canonicalize dependency result alias exports";
                          let events = parse_jsonl (stdout_contents ()) in
                          let file_paths =
                            events
                            |> List.filter_map
                              (fun json ->
                                match Data.Json.get_field "type" json with
                                | Some (Data.Json.String "check_file") -> (
                                    match Data.Json.get_field "result" json with
                                    | Some result_json -> Data.Json.get_field "path" result_json
                                    | None -> None
                                  )
                                | _ -> None)
                          in
                          let diagnostic_count =
                            events
                            |> List.filter
                              (fun json ->
                                match Data.Json.get_field "type" json with
                                | Some (Data.Json.String "check_diagnostic") -> true
                                | _ -> false)
                            |> List.length
                          in
                          Test.assert_equal
                            ~expected:[ Data.Json.String "packages/colors/src/colors.ml" ]
                            ~actual:file_paths;
                          Test.assert_equal ~expected:0 ~actual:diagnostic_count;
                          Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                          Ok ()
                    )
                )
            )
        ))

let test_check_package_filter_merges_bootstrap_and_dependency_module_exports = fun _ctx ->
  with_tempdir_result "riot_check_bootstrap_shadow"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let shadow_root = Path.(workspace_root / Path.v "packages/shadow") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      let shadow_source = Path.(shadow_root / Path.v "src/int.ml") in
      let app_source = Path.(app_root / Path.v "src/app.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"shadow"
            ~path:shadow_root
            ~relative_path:(Path.v "packages/shadow")
            ~sources:{ empty_sources with src = [ Path.v "src/int.ml" ] }
            ();
          make_package
            ~name:"app"
            ~path:app_root
            ~relative_path:(Path.v "packages/app")
            ~dependencies:[ make_dependency "shadow" ]
            ~sources:{ empty_sources with src = [ Path.v "src/app.ml" ] }
            ();
        ] in
      match write_file shadow_source "let sentinel = 21\n" with
      | Error err -> Error err
      | Ok () ->
          match write_file app_source "let rendered = Int.to_string Int.sentinel\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check [ "check"; "--json"; "-p"; "app" ] |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              match Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches with
              | Error exn -> Error ("package check should merge dependency and bootstrap module exports: "
              ^ Exception.to_string exn
              ^ "\nstdout:\n"
              ^ stdout_contents ()
              ^ "\nstderr:\n"
              ^ stderr_contents ())
              | Ok () ->
                  let events = parse_jsonl (stdout_contents ()) in
                  let file_paths =
                    events
                    |> List.filter_map
                      (fun json ->
                        match Data.Json.get_field "type" json with
                        | Some (Data.Json.String "check_file") -> (
                            match Data.Json.get_field "result" json with
                            | Some result_json -> Data.Json.get_field "path" result_json
                            | None -> None
                          )
                        | _ -> None)
                  in
                  let diagnostic_count =
                    events
                    |> List.filter
                      (fun json ->
                        match Data.Json.get_field "type" json with
                        | Some (Data.Json.String "check_diagnostic") -> true
                        | _ -> false)
                    |> List.length
                  in
                  Test.assert_equal
                    ~expected:[ Data.Json.String "packages/app/src/app.ml" ]
                    ~actual:file_paths;
                  Test.assert_equal ~expected:0 ~actual:diagnostic_count;
                  Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                  Ok ())

let test_check_requires_workspace = fun _ctx ->
  with_tempdir_result "riot_check_requires_workspace"
    (fun tmpdir ->
      with_current_dir_result tmpdir
        (fun () ->
          match Riot_cli.Cli.run ~args:[ "riot"; "check"; "--json"; "-p"; "colors" ] with
          | Ok () ->
              Error "expected check outside a workspace to fail"
          | Error (Failure message) ->
              Test.assert_equal ~expected:"Not in a riot workspace" ~actual:message;
              Ok ()
          | Error exn ->
              Error ("unexpected error: " ^ Exception.to_string exn)))

let tests =
  Test.[
    case "check: parse --json flag" test_check_accepts_json_flag;
    case "check: parse --quiet flag" test_check_accepts_quiet_flag;
    case "check: parse --explain option" test_check_accepts_explain_option;
    case "check: parse --package option" test_check_accepts_package_option;
    case "check: parse path argument" test_check_accepts_path_argument;
    case "check: parse multiple path arguments" test_check_accepts_multiple_paths;
    case "check: parse without path" test_check_accepts_no_path_without_explain;
    case "check: json streams a single explicit file without duplicates" test_check_json_streams_single_explicit_file_without_duplicates;
    case "check: human output snapshot" test_check_human_output_snapshot;
    case "check: clean success shows package progress" test_check_success_emits_package_progress;
    case "check: package filter limits workspace scan" test_check_package_filter_limits_workspace_scan;
    case "check: package filter handles hyphenated package names" test_check_package_filter_handles_hyphenated_package_names;
    case "check: explicit path handles hyphenated package names" test_check_explicit_path_handles_hyphenated_package_names;
    case "check: explicit file handles hyphenated package names" test_check_explicit_file_handles_hyphenated_package_names;
    case "check: package filter handles hyphenated package names after workspace prepare" test_check_package_filter_handles_hyphenated_package_names_after_workspace_prepare;
    case "check: json includes typ event diagnostics" test_check_json_includes_typ_event_diagnostics;
    case "check: package filter uses sibling source exports during package scans" test_check_package_filter_uses_package_session_for_cross_file_exports;
    case "check: package filter emits authoritative package engine events" test_check_package_filter_emits_authoritative_package_engine_event;
    case "check: package filter avoids rooted session reconstruction events" test_check_package_filter_does_not_emit_rooted_session_reconstruction_events;
    case "check: package filter uses sibling source record types during package scans" test_check_package_filter_uses_package_session_for_cross_file_record_types;
    case "check: package filter loads workspace dependency summaries" test_check_package_filter_loads_workspace_dependency_summaries;
    case "check: package filter emits cached dependency package events" test_check_package_filter_emits_cached_dependency_package_events;
    case "check: package filter human output shows cached dependency progress" test_check_package_filter_human_output_shows_cached_dependency_progress;
    case "check: package filter loads external dependency summaries" test_check_package_filter_loads_external_dependency_summaries;
    case "check: package filter persists module typings to store" test_check_package_filter_persists_module_typings_to_store;
    case "check: package filter persists interface-shaped module typings" test_check_package_filter_persists_interface_shaped_module_typings;
    case "check: package filter reports signature inclusion errors" test_check_package_filter_reports_signature_inclusion_errors;
    case "check: match coverage warnings stream without failing riot check" test_check_match_coverage_warnings_do_not_fail;
    case "check: package filter reexports workspace dependency summaries via include" test_check_package_filter_reexports_workspace_dependency_summaries_via_include;
    case "check: package filter persists locally built dependency modules" test_check_package_filter_persists_locally_built_dependency_modules;
    case "check: package filter loads dependency library reexports from sibling sources" test_check_package_filter_loads_dependency_library_reexports_from_sibling_sources;
    case "check: package filter uses sibling dependency record reexports during package scans" test_check_package_filter_uses_sibling_reexported_dependency_record_types;
    case "check: package filter reexports same-named workspace modules via alias" test_check_package_filter_reexports_same_named_workspace_modules_via_alias;
    case "check: package filter preserves nested same-named alias reexports" test_check_package_filter_preserves_nested_same_named_alias_reexports;
    case "check: package filter preserves directory same-named alias reexports" test_check_package_filter_preserves_directory_same_named_alias_reexports;
    case "check: expansive bindings stay monomorphic through riot check" test_check_expansive_bindings_stay_monomorphic;
    case "check: relaxed value restriction keeps covariant lists polymorphic through riot check" test_check_relaxed_value_restriction_preserves_covariant_lists;
    case
      "check: relaxed value restriction keeps covariant nominal types polymorphic through riot check"
      test_check_relaxed_value_restriction_preserves_covariant_nominal_types;
    case "check: package filter loads transitive workspace dependency summaries" test_check_package_filter_loads_transitive_workspace_dependency_summaries;
    case "check: package filter keeps dependency summaries when dependency has broken sources" test_check_package_filter_keeps_dependency_summaries_when_dependency_has_broken_sources;
    case "check: package filter uses dependency sibling interface alias exports" test_check_package_filter_uses_dependency_sibling_interface_alias_exports;
    case "check: package filter preserves dependency nested alias exports" test_check_package_filter_preserves_dependency_nested_alias_exports;
    case "check: package filter canonicalizes dependency result alias exports" test_check_package_filter_canonicalizes_dependency_result_alias_exports;
    case "check: explicit workspace file uses sibling source exports" test_check_explicit_workspace_file_uses_sibling_source_exports;
    case "check: explicit relative workspace file uses sibling source exports" test_check_explicit_relative_workspace_file_uses_sibling_source_exports;
    case "check: explicit workspace file loads dependency summaries" test_check_explicit_workspace_file_loads_dependency_summaries;
    case "check: explicit workspace file loads transitive dependency summaries" test_check_explicit_workspace_file_loads_transitive_dependency_summaries;
    case "check: package filter merges bootstrap and dependency module exports" test_check_package_filter_merges_bootstrap_and_dependency_module_exports;
    case "check: requires workspace" test_check_requires_workspace;
  ]

let name = "Riot CLI Check Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
