open Std
open Std.Result.Syntax

module Test = Std.Test

open Riot_build2

let package = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let kernel_package = package "kernel"

let clone_workspace_with_target = fun (workspace: Riot_model.Workspace.t) ~target_dir ->
  Riot_model.Workspace.make
    ?name:workspace.name
    ~root:workspace.root
    ~packages:workspace.packages
    ~dependencies:workspace.dependencies
    ~dev_dependencies:workspace.dev_dependencies
    ~build_dependencies:workspace.build_dependencies
    ~profile_overrides:workspace.profile_overrides
    ~source_ignore_patterns:workspace.source_ignore_patterns
    ~target_dir
    ()

let load_workspace = fun () ->
  Workspace_loader.load_local ~root:(Path.v ".")
  |> Result.map_err ~fn:Workspace_loader.error_message

let with_kernel_workspace = fun fn ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_kernel"
    (fun tempdir ->
      let* workspace = load_workspace () in
      let workspace =
        clone_workspace_with_target workspace ~target_dir:Path.(tempdir / Path.v "target")
      in
      fn workspace) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let with_kernel_workspace_target = fun target_dir fn ->
  let* workspace = load_workspace () in
  clone_workspace_with_target workspace ~target_dir
  |> fn

let rec copy_tree = fun ~src ~dst ->
  let* () =
    Fs.create_dir_all dst
    |> Result.map_err ~fn:IO.error_message
  in
  let* entries =
    Fs.read_dir src
    |> Result.map_err ~fn:IO.error_message
  in
  let rec loop () =
    match Iter.MutIterator.next entries with
    | None -> Ok ()
    | Some entry ->
        let entry_name = Path.v (Path.basename entry) in
        let src_entry =
          if Path.is_absolute entry then
            entry
          else
            Path.(src / entry)
        in
        let dst_entry = Path.(dst / entry_name) in
        let* is_dir =
          Fs.is_dir src_entry
          |> Result.map_err ~fn:IO.error_message
        in
        let* () =
          if is_dir then
            copy_tree ~src:src_entry ~dst:dst_entry
          else
            Fs.copy ~src:src_entry ~dst:dst_entry
            |> Result.map_err ~fn:IO.error_message
        in
        loop ()
  in
  loop ()

let isolated_kernel_workspace = fun tempdir ->
  let* workspace = load_workspace () in
  match List.find
    workspace.packages
    ~fn:(fun (manifest: Riot_model.Package_manifest.t) ->
      Riot_model.Package_name.equal
        manifest.name
        kernel_package) with
  | None -> Error "expected workspace to contain kernel package"
  | Some manifest ->
      let package_root = Path.(tempdir / Path.v "kernel") in
      let* () = copy_tree ~src:manifest.path ~dst:package_root in
      Ok (Riot_model.Workspace.make
        ?name:workspace.name
        ~root:tempdir
        ~packages:[ { manifest with path = package_root; relative_path = Path.v "kernel" } ]
        ~dependencies:workspace.dependencies
        ~dev_dependencies:workspace.dev_dependencies
        ~build_dependencies:workspace.build_dependencies
        ~profile_overrides:workspace.profile_overrides
        ~source_ignore_patterns:workspace.source_ignore_patterns
        ~target_dir:Path.(tempdir / Path.v "target")
        ())

let with_isolated_kernel_workspace = fun fn ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_kernel_incremental"
    (fun tempdir ->
      let* workspace = isolated_kernel_workspace tempdir in
      fn workspace) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let mutate_kernel_event_source = fun workspace revision ->
  let path =
    Path.(workspace.Riot_model.Workspace.root / Path.v "kernel" / Path.v "src/async/event.ml")
  in
  let marker =
    "\nlet __riot_build2_partial_rebuild_marker_"
    ^ Int.to_string revision
    ^ " = "
    ^ Int.to_string revision
    ^ "\n"
  in
  let* content =
    Fs.read_to_string path
    |> Result.map_err ~fn:IO.error_message
  in
  Fs.write (content ^ marker) path
  |> Result.map_err ~fn:IO.error_message

let relative_target_dir = fun name ->
  Path.(Path.v "_build"
  / Path.v "riot-build2-tests"
  / Path.v (name ^ "-" ^ Riot_model.Session_id.to_string (Riot_model.Session_id.make ())))

let current_target = fun () -> Riot_model.Target.current

let is_kernel_build = fun (build: Goal.build_package) ->
  Riot_model.Package_name.equal build.package kernel_package

let profile_equal = fun (left: Riot_model.Profile.t) (right: Riot_model.Profile.t) ->
  String.equal left.name right.name

let build_matrix_targets = fun workspace ->
  let toolchain_config = Riot_model.Toolchain_config.from_root ~root:workspace.Riot_model.Workspace.root in
  let configured =
    Riot_model.Target.configured_targets ~host:(current_target ()) toolchain_config
    |> Riot_model.Target.Set.to_list
  in
  let installed =
    configured
    |> List.filter
      ~fn:(fun target ->
        let ocamlopt =
          Path.(Riot_model.Riot_dirs.dot_riot
          / Path.v "toolchains"
          / Path.v toolchain_config.version
          / Path.v (Riot_model.Target.to_string target)
          / Path.v "bin/ocamlopt.opt")
        in
        match Fs.exists ocamlopt with
        | Ok true -> true
        | Ok false
        | Error _ -> false)
  in
  let host = current_target () in
  let non_host =
    installed
    |> List.filter ~fn:(fun target -> not (Riot_model.Target.equal target host))
  in
  match non_host with
  | target :: _ -> Ok [ host; target ]
  | [] ->
      Error "expected at least one installed cross toolchain for kernel multi-target build test"

let build_result_matches = fun package profile target result ->
  Riot_model.Package_name.equal result.Build_result.package package
  && profile_equal result.profile profile
  && Riot_model.Target.equal result.target target

let expect_successful_package_status = fun result ->
  match result.Build_result.status with
  | Build_result.Built _
  | Cached _ -> Ok ()
  | Failed error -> Error ("package build failed: " ^ Error.message error)

let expect_kernel_build_matrix_results = fun result ~profiles ~targets ->
  let package_results = Build_result.package_results result in
  let expected_count = List.length profiles * List.length targets in
  let kernel_results =
    package_results
    |> List.filter
      ~fn:(fun result -> Riot_model.Package_name.equal result.Build_result.package kernel_package)
  in
  let actual_count = List.length kernel_results in
  if not (Int.equal actual_count expected_count) then
    Error ("expected kernel matrix build to produce "
    ^ Int.to_string expected_count
    ^ " package results, got "
    ^ Int.to_string actual_count)
  else
    let expected =
      profiles
      |> List.flat_map
        ~fn:(fun profile ->
          List.map targets ~fn:(fun target -> (profile, target)))
    in
    let missing =
      expected
      |> List.filter
        ~fn:(fun (profile, target) ->
          not (
            List.any
              kernel_results
              ~fn:(build_result_matches kernel_package profile target)
          ))
    in
    match missing with
    | _ :: _ -> Error "kernel matrix build missed at least one profile/target result"
    | [] ->
        kernel_results
        |> List.fold_left
          ~init:(Ok ())
          ~fn:(fun acc result ->
            let* () = acc in
            expect_successful_package_status result)

let expect_kernel_build_goal = fun actual ->
  match actual with
  | [
      Goal.BuildPackage {
        package;
        scope;
        profile;
        target;
      };
    ] when Riot_model.Package_name.equal package kernel_package
  && scope = Goal.Runtime
  && profile = Riot_model.Profile.debug
  && Riot_model.Target.equal target (current_target ()) -> Ok ()
  | _ -> Error "expected kernel build intent to expand to one concrete kernel build goal"

let test_kernel_intent_plans_concrete_package_goal = fun _ctx ->
  with_kernel_workspace
    (fun workspace ->
      let catalog = Package_catalog.create workspace in
      User_intent.build
        ~packages:(User_intent.NamedPackages [ kernel_package ])
        ~targets:(User_intent.ManyTargets [ current_target () ])
        ~profiles:(User_intent.ManyProfiles [ Riot_model.Profile.debug ])
        ()
      |> Intent_planner.expand catalog
      |> Result.map_err ~fn:Error.message
      |> Result.and_then ~fn:expect_kernel_build_goal)

let summary_errors = fun summary ->
  summary.Executor.Summary.results
  |> List.filter_map
    ~fn:(fun result ->
      result.Executor.Summary.error
      |> Option.map ~fn:Error.message)
  |> String.concat "\n"

let completed_kind = fun summary ~fn ->
  summary.Executor.Summary.results
  |> List.any
    ~fn:(fun result ->
      result.Executor.Summary.status = Work_node.Completed && fn (Work_node.kind result.node))

let kind_name = fun __tmp1 ->
  match __tmp1 with
  | Work_node.UserIntent _ -> "UserIntent"
  | Goal _ -> "Goal"
  | ToolchainReady _ -> "ToolchainReady"
  | SourceAnalysis _ -> "SourceAnalysis"
  | PackageArtifact _ -> "PackageArtifact"
  | PackageFinalize _ -> "PackageFinalize"
  | ModulePlan _ -> "ModulePlan"
  | ActionPlan _ -> "ActionPlan"
  | ModuleDependencies _ -> "ModuleDependencies"
  | OCamlInterface _ -> "OCamlInterface"
  | OCamlByteImplementation _ -> "OCamlByteImplementation"
  | OCamlImplementation _ -> "OCamlImplementation"
  | OCamlGenerated _ -> "OCamlGenerated"
  | CObject _ -> "CObject"
  | OCamlLibrary _ -> "OCamlLibrary"
  | OCamlArchive _ -> "OCamlArchive"
  | ActionExecution _ -> "ActionExecution"

let completed_kind_names = fun summary ->
  summary.Executor.Summary.results
  |> List.filter_map
    ~fn:(fun result ->
      if result.Executor.Summary.status = Work_node.Completed then
        Some (kind_name (Work_node.kind result.node))
      else
        None)
  |> List.sort ~compare:String.compare
  |> List.unique ~compare:String.compare
  |> String.concat ", "

let expect_completed_kind = fun summary label fn ->
  if completed_kind summary ~fn then
    Ok ()
  else
    Error ("expected completed " ^ label ^ " node; completed kinds: " ^ completed_kind_names summary)

let completed_node = fun summary ~fn ->
  summary.Executor.Summary.results
  |> List.find
    ~fn:(fun result ->
      result.Executor.Summary.status = Work_node.Completed && fn (Work_node.kind result.node))
  |> Option.map ~fn:(fun result -> result.Executor.Summary.node)

let require_completed_node = fun summary label fn ->
  match completed_node summary ~fn with
  | Some node -> Ok node
  | None ->
      Error ("expected completed "
      ^ label
      ^ " node; completed kinds: "
      ^ completed_kind_names summary)

let node_by_id = fun summary id ->
  summary.Executor.Summary.results
  |> List.find
    ~fn:(fun result -> Work_node.Node_id.equal (Work_node.id result.Executor.Summary.node) id)
  |> Option.map ~fn:(fun result -> result.Executor.Summary.node)

let node_depends_on = fun summary node ~dependency ->
  Work_node.dependencies node
  |> List.any
    ~fn:(fun dependency_id ->
      match node_by_id summary dependency_id with
      | Some dependency_node -> dependency (Work_node.kind dependency_node)
      | None -> false)

let expect_dependency = fun summary ~from_label ~from ~to_label ~dependency ->
  if node_depends_on summary from ~dependency then
    Ok ()
  else
    Error ("expected " ^ from_label ^ " to depend on " ^ to_label)

let completed_kind_count = fun summary ~fn ->
  summary.Executor.Summary.results
  |> List.filter
    ~fn:(fun result ->
      result.Executor.Summary.status = Work_node.Completed && fn (Work_node.kind result.node))
  |> List.length

let kernel_action_results = fun executor ->
  Build_services.action_results executor
  |> List.filter
    ~fn:(fun result ->
      Riot_model.Package_name.equal
        result.Action_execution.ref_.package
        kernel_package)

let action_result_status_matches = fun result status ->
  match (result.Action_execution.status, status) with
  | (Action_execution.Cached _, `Cached)
  | (Executed _, `Executed)
  | (Failed _, `Failed) -> true
  | (Cached _, _)
  | (Executed _, _)
  | (Failed _, _) -> false

let action_result_status_count = fun results status ->
  results
  |> List.filter ~fn:(fun result -> action_result_status_matches result status)
  |> List.length

let action_kind_status_count = fun results ~kind ~status ->
  results
  |> List.filter
    ~fn:(fun result ->
      String.equal result.Action_execution.action_kind kind
      && action_result_status_matches result status)
  |> List.length

let cached_action_cache_promotion_count = fun results ->
  results
  |> List.filter
    ~fn:(fun result ->
      action_result_status_matches result `Cached
      && not (Time.Duration.is_zero result.Action_execution.timing.cache_promotion))
  |> List.length

let expect_cached_kernel_actions_restore_outputs_to_package_sandbox = fun executor ->
  let results = kernel_action_results executor in
  let cached_actions = action_result_status_count results `Cached in
  let promoted_cached_actions = cached_action_cache_promotion_count results in
  if Int.equal promoted_cached_actions cached_actions then
    Ok ()
  else
    Error ("expected cached action hits to restore declared outputs into the package sandbox, but "
    ^ Int.to_string promoted_cached_actions
    ^ " of "
    ^ Int.to_string cached_actions
    ^ " cached actions recorded cache promotion work")

let kernel_action_result_counts = fun executor _summary ->
  let results = kernel_action_results executor in
  [
    ("kernel action results", List.length results);
    ("kernel action results cached", action_result_status_count results `Cached);
    ("kernel action results executed", action_result_status_count results `Executed);
    ("kernel action results failed", action_result_status_count results `Failed);
    ("kernel action results missing from summary", 0);
    (
      "kernel CompileC cached",
      action_kind_status_count results ~kind:"CompileC" ~status:`Cached
    );
    (
      "kernel CompileC executed",
      action_kind_status_count results ~kind:"CompileC" ~status:`Executed
    );
    (
      "kernel CompileC failed",
      action_kind_status_count results ~kind:"CompileC" ~status:`Failed
    );
    (
      "kernel CompileSource cached",
      action_kind_status_count results ~kind:"CompileSource" ~status:`Cached
    );
    (
      "kernel CompileSource executed",
      action_kind_status_count results ~kind:"CompileSource" ~status:`Executed
    );
    (
      "kernel CompileSource failed",
      action_kind_status_count results ~kind:"CompileSource" ~status:`Failed
    );
    (
      "kernel CompileInterface cached",
      action_kind_status_count results ~kind:"CompileInterface" ~status:`Cached
    );
    (
      "kernel CompileInterface executed",
      action_kind_status_count results ~kind:"CompileInterface" ~status:`Executed
    );
    (
      "kernel CompileInterface failed",
      action_kind_status_count results ~kind:"CompileInterface" ~status:`Failed
    );
    (
      "kernel CompileByteImplementation cached",
      action_kind_status_count results ~kind:"CompileByteImplementation" ~status:`Cached
    );
    (
      "kernel CompileByteImplementation executed",
      action_kind_status_count results ~kind:"CompileByteImplementation" ~status:`Executed
    );
    (
      "kernel CompileByteImplementation failed",
      action_kind_status_count results ~kind:"CompileByteImplementation" ~status:`Failed
    );
    (
      "kernel CompileNativeImplementation cached",
      action_kind_status_count results ~kind:"CompileNativeImplementation" ~status:`Cached
    );
    (
      "kernel CompileNativeImplementation executed",
      action_kind_status_count results ~kind:"CompileNativeImplementation" ~status:`Executed
    );
    (
      "kernel CompileNativeImplementation failed",
      action_kind_status_count results ~kind:"CompileNativeImplementation" ~status:`Failed
    );
    (
      "kernel CompileSources cached",
      action_kind_status_count results ~kind:"CompileSources" ~status:`Cached
    );
    (
      "kernel CompileSources executed",
      action_kind_status_count results ~kind:"CompileSources" ~status:`Executed
    );
    (
      "kernel CompileSources failed",
      action_kind_status_count results ~kind:"CompileSources" ~status:`Failed
    );
    (
      "kernel final archive cached",
      action_kind_status_count results ~kind:"CompileLibrary" ~status:`Cached
    );
    (
      "kernel final archive executed",
      action_kind_status_count results ~kind:"CompileLibrary" ~status:`Executed
    );
    (
      "kernel final archive failed",
      action_kind_status_count results ~kind:"CompileLibrary" ~status:`Failed
    );
  ]

let count_rows_to_string = fun rows ->
  rows
  |> List.map ~fn:(fun (label, count) -> label ^ "=" ^ Int.to_string count)
  |> String.concat ", "

let expect_exact_counts = fun ~expected actual ->
  let rec loop mismatches actual expected =
    match (actual, expected) with
    | ([], []) -> List.reverse mismatches
    | (
        (actual_label, actual_count) :: actual_rest,
        (expected_label, expected_count) :: expected_rest
      ) ->
        let mismatch =
          if String.equal actual_label expected_label && Int.equal actual_count expected_count then
            None
          else
            Some (expected_label
            ^ " expected "
            ^ Int.to_string expected_count
            ^ " but got "
            ^ actual_label
            ^ "="
            ^ Int.to_string actual_count)
        in
        loop (Option.to_list mismatch @ mismatches) actual_rest expected_rest
    | ([], remaining_expected) ->
        loop
          (("missing expected counts: " ^ count_rows_to_string remaining_expected) :: mismatches)
          []
          []
    | (remaining_actual, []) ->
        loop
          (("unexpected extra counts: " ^ count_rows_to_string remaining_actual) :: mismatches)
          []
          []
  in
  match loop [] actual expected with
  | [] -> Ok ()
  | mismatches ->
      Error ("kernel build graph counts changed:\n"
      ^ String.concat "\n" mismatches
      ^ "\nactual: "
      ^ count_rows_to_string actual)

let kernel_cold_graph_counts = fun summary ->
  [
    ("summary.results", List.length summary.Executor.Summary.results);
    ("summary.completed_count", summary.Executor.Summary.completed_count);
    ("summary.failed_count", summary.Executor.Summary.failed_count);
    ("UserIntent nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.UserIntent _ -> true
        | _ -> false));
    ("Goal nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.Goal (Goal.BuildPackage { package; _ }) ->
            Riot_model.Package_name.equal package kernel_package
        | _ -> false));
    ("PackageArtifact nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.PackageArtifact build ->
            Riot_model.Package_name.equal build.package kernel_package
        | _ -> false));
    ("PackageFinalize nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.PackageFinalize build ->
            Riot_model.Package_name.equal build.package kernel_package
        | _ -> false));
    ("ActionPlan nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ActionPlan build -> Riot_model.Package_name.equal build.package kernel_package
        | _ -> false));
    ("ModulePlan nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ModulePlan build -> Riot_model.Package_name.equal build.package kernel_package
        | _ -> false));
    ("ModuleDependencies nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ModuleDependencies build -> is_kernel_build build
        | _ -> false));
    ("SourceAnalysis nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.SourceAnalysis source ->
            Riot_model.Package_name.equal source.key.package kernel_package
        | _ -> false));
    ("ToolchainReady nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ToolchainReady toolchain ->
            Riot_model.Target.equal toolchain.target (current_target ())
        | _ -> false));
    ("OCamlLibrary nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlLibrary action ->
            Riot_model.Package_name.equal action.ref_.package kernel_package
        | _ -> false));
    ("OCamlInterface nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlInterface source -> is_kernel_build source.Rule.build
        | _ -> false));
    ("OCamlByteImplementation nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlByteImplementation source -> is_kernel_build source.Rule.build
        | _ -> false));
    ("OCamlImplementation nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlImplementation source -> is_kernel_build source.Rule.build
        | _ -> false));
    ("OCamlGenerated nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlGenerated source -> is_kernel_build source.Rule.build
        | _ -> false));
    ("CObject nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.CObject c_object -> is_kernel_build c_object.Rule.build
        | _ -> false));
    ("OCamlArchive nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlArchive build -> is_kernel_build build
        | _ -> false));
    ("ActionExecution nodes", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ActionExecution action ->
            Riot_model.Package_name.equal action.ref_.package kernel_package
        | _ -> false));
    ("kernel compiler actions", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlInterface source
        | Work_node.OCamlByteImplementation source
        | Work_node.OCamlImplementation source -> is_kernel_build source.Rule.build
        | OCamlGenerated source -> is_kernel_build source.Rule.build
        | CObject c_object -> is_kernel_build c_object.Rule.build
        | OCamlArchive build -> is_kernel_build build
        | _ -> false));
    ("kernel CompileC actions", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.CObject c_object -> is_kernel_build c_object.Rule.build
        | _ -> false));
    ("kernel CompileSource actions", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlInterface source
        | Work_node.OCamlByteImplementation source
        | Work_node.OCamlImplementation source -> is_kernel_build source.Rule.build
        | OCamlGenerated source -> is_kernel_build source.Rule.build
        | _ -> false));
    ("kernel CompileSources actions", 0);
    ("kernel interface CompileSource actions", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlInterface source -> is_kernel_build source.Rule.build
        | _ -> false));
    ("kernel byte CompileSource actions", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlByteImplementation source -> is_kernel_build source.Rule.build
        | _ -> false));
    ("kernel implementation CompileSource actions", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlImplementation source -> is_kernel_build source.Rule.build
        | _ -> false));
    ("kernel generated CompileSource actions", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlGenerated source -> is_kernel_build source.Rule.build
        | _ -> false));
    ("kernel final archive actions", completed_kind_count
      summary
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlArchive build -> is_kernel_build build
        | _ -> false));
  ]

let expected_kernel_cold_graph_counts = [
  ("summary.results", 465);
  ("summary.completed_count", 465);
  ("summary.failed_count", 0);
  ("UserIntent nodes", 1);
  ("Goal nodes", 1);
  ("PackageArtifact nodes", 1);
  ("PackageFinalize nodes", 0);
  ("ActionPlan nodes", 0);
  ("ModulePlan nodes", 0);
  ("ModuleDependencies nodes", 1);
  ("SourceAnalysis nodes", 206);
  ("ToolchainReady nodes", 1);
  ("OCamlLibrary nodes", 0);
  ("OCamlInterface nodes", 86);
  ("OCamlByteImplementation nodes", 3);
  ("OCamlImplementation nodes", 89);
  ("OCamlGenerated nodes", 62);
  ("CObject nodes", 13);
  ("OCamlArchive nodes", 1);
  ("ActionExecution nodes", 0);
  ("kernel compiler actions", 254);
  ("kernel CompileC actions", 13);
  ("kernel CompileSource actions", 240);
  ("kernel CompileSources actions", 0);
  ("kernel interface CompileSource actions", 86);
  ("kernel byte CompileSource actions", 3);
  ("kernel implementation CompileSource actions", 89);
  ("kernel generated CompileSource actions", 62);
  ("kernel final archive actions", 1);
]

let expected_kernel_event_change_graph_counts = [
  ("summary.results", 466);
  ("summary.completed_count", 466);
  ("summary.failed_count", 0);
  ("UserIntent nodes", 1);
  ("Goal nodes", 1);
  ("PackageArtifact nodes", 1);
  ("PackageFinalize nodes", 0);
  ("ActionPlan nodes", 0);
  ("ModulePlan nodes", 0);
  ("ModuleDependencies nodes", 1);
  ("SourceAnalysis nodes", 206);
  ("ToolchainReady nodes", 1);
  ("OCamlLibrary nodes", 0);
  ("OCamlInterface nodes", 86);
  ("OCamlByteImplementation nodes", 3);
  ("OCamlImplementation nodes", 89);
  ("OCamlGenerated nodes", 63);
  ("CObject nodes", 13);
  ("OCamlArchive nodes", 1);
  ("ActionExecution nodes", 0);
  ("kernel compiler actions", 255);
  ("kernel CompileC actions", 13);
  ("kernel CompileSource actions", 241);
  ("kernel CompileSources actions", 0);
  ("kernel interface CompileSource actions", 86);
  ("kernel byte CompileSource actions", 3);
  ("kernel implementation CompileSource actions", 89);
  ("kernel generated CompileSource actions", 63);
  ("kernel final archive actions", 1);
]

let expect_kernel_exact_cold_graph_counts = fun summary ->
  expect_exact_counts
    ~expected:expected_kernel_cold_graph_counts
    (kernel_cold_graph_counts summary)

let expect_kernel_exact_event_change_graph_counts = fun summary ->
  expect_exact_counts
    ~expected:expected_kernel_event_change_graph_counts
    (kernel_cold_graph_counts summary)

let expect_kernel_exact_isolated_graph_counts = fun summary ->
  expect_exact_counts
    ~expected:expected_kernel_event_change_graph_counts
    (kernel_cold_graph_counts summary)

let expected_kernel_cold_action_result_counts = [
  ("kernel action results", 254);
  ("kernel action results cached", 0);
  ("kernel action results executed", 254);
  ("kernel action results failed", 0);
  ("kernel action results missing from summary", 0);
  ("kernel CompileC cached", 0);
  ("kernel CompileC executed", 13);
  ("kernel CompileC failed", 0);
  ("kernel CompileSource cached", 0);
  ("kernel CompileSource executed", 0);
  ("kernel CompileSource failed", 0);
  ("kernel CompileInterface cached", 0);
  ("kernel CompileInterface executed", 86);
  ("kernel CompileInterface failed", 0);
  ("kernel CompileByteImplementation cached", 0);
  ("kernel CompileByteImplementation executed", 34);
  ("kernel CompileByteImplementation failed", 0);
  ("kernel CompileNativeImplementation cached", 0);
  ("kernel CompileNativeImplementation executed", 120);
  ("kernel CompileNativeImplementation failed", 0);
  ("kernel CompileSources cached", 0);
  ("kernel CompileSources executed", 0);
  ("kernel CompileSources failed", 0);
  ("kernel final archive cached", 0);
  ("kernel final archive executed", 1);
  ("kernel final archive failed", 0);
]

let expect_kernel_exact_cold_action_result_counts = fun executor summary ->
  expect_exact_counts
    ~expected:expected_kernel_cold_action_result_counts
    (kernel_action_result_counts executor summary)

let expected_kernel_warm_graph_counts = [
  ("summary.results", 3);
  ("summary.completed_count", 3);
  ("summary.failed_count", 0);
  ("UserIntent nodes", 1);
  ("Goal nodes", 1);
  ("PackageArtifact nodes", 1);
  ("PackageFinalize nodes", 0);
  ("ActionPlan nodes", 0);
  ("ModulePlan nodes", 0);
  ("ModuleDependencies nodes", 0);
  ("SourceAnalysis nodes", 0);
  ("ToolchainReady nodes", 0);
  ("OCamlLibrary nodes", 0);
  ("OCamlInterface nodes", 0);
  ("OCamlByteImplementation nodes", 0);
  ("OCamlImplementation nodes", 0);
  ("OCamlGenerated nodes", 0);
  ("CObject nodes", 0);
  ("OCamlArchive nodes", 0);
  ("ActionExecution nodes", 0);
  ("kernel compiler actions", 0);
  ("kernel CompileC actions", 0);
  ("kernel CompileSource actions", 0);
  ("kernel CompileSources actions", 0);
  ("kernel interface CompileSource actions", 0);
  ("kernel byte CompileSource actions", 0);
  ("kernel implementation CompileSource actions", 0);
  ("kernel generated CompileSource actions", 0);
  ("kernel final archive actions", 0);
]

let expect_kernel_exact_warm_graph_counts = fun summary ->
  expect_exact_counts
    ~expected:expected_kernel_warm_graph_counts
    (kernel_cold_graph_counts summary)

let expected_kernel_warm_action_result_counts = [
  ("kernel action results", 0);
  ("kernel action results cached", 0);
  ("kernel action results executed", 0);
  ("kernel action results failed", 0);
  ("kernel action results missing from summary", 0);
  ("kernel CompileC cached", 0);
  ("kernel CompileC executed", 0);
  ("kernel CompileC failed", 0);
  ("kernel CompileSource cached", 0);
  ("kernel CompileSource executed", 0);
  ("kernel CompileSource failed", 0);
  ("kernel CompileInterface cached", 0);
  ("kernel CompileInterface executed", 0);
  ("kernel CompileInterface failed", 0);
  ("kernel CompileByteImplementation cached", 0);
  ("kernel CompileByteImplementation executed", 0);
  ("kernel CompileByteImplementation failed", 0);
  ("kernel CompileNativeImplementation cached", 0);
  ("kernel CompileNativeImplementation executed", 0);
  ("kernel CompileNativeImplementation failed", 0);
  ("kernel CompileSources cached", 0);
  ("kernel CompileSources executed", 0);
  ("kernel CompileSources failed", 0);
  ("kernel final archive cached", 0);
  ("kernel final archive executed", 0);
  ("kernel final archive failed", 0);
]

let expect_kernel_exact_warm_action_result_counts = fun executor summary ->
  expect_exact_counts
    ~expected:expected_kernel_warm_action_result_counts
    (kernel_action_result_counts executor summary)

let expected_kernel_event_change_action_result_counts = [
  ("kernel action results", 254);
  ("kernel action results cached", 252);
  ("kernel action results executed", 2);
  ("kernel action results failed", 0);
  ("kernel action results missing from summary", 0);
  ("kernel CompileC cached", 13);
  ("kernel CompileC executed", 0);
  ("kernel CompileC failed", 0);
  ("kernel CompileSource cached", 0);
  ("kernel CompileSource executed", 0);
  ("kernel CompileSource failed", 0);
  ("kernel CompileInterface cached", 86);
  ("kernel CompileInterface executed", 0);
  ("kernel CompileInterface failed", 0);
  ("kernel CompileByteImplementation cached", 34);
  ("kernel CompileByteImplementation executed", 0);
  ("kernel CompileByteImplementation failed", 0);
  ("kernel CompileNativeImplementation cached", 119);
  ("kernel CompileNativeImplementation executed", 1);
  ("kernel CompileNativeImplementation failed", 0);
  ("kernel CompileSources cached", 0);
  ("kernel CompileSources executed", 0);
  ("kernel CompileSources failed", 0);
  ("kernel final archive cached", 0);
  ("kernel final archive executed", 1);
  ("kernel final archive failed", 0);
]

let expect_kernel_event_change_action_result_counts = fun executor summary ->
  expect_exact_counts
    ~expected:expected_kernel_event_change_action_result_counts
    (kernel_action_result_counts executor summary)

let expect_kernel_graph_shape = fun result ->
  let summary = result.Build_result.summary in
  let* intent =
    require_completed_node
      summary
      "UserIntent"
      (fun __tmp1 ->
        match __tmp1 with
        | Work_node.UserIntent _ -> true
        | _ -> false)
  in
  let* goal =
    require_completed_node
      summary
      "Goal"
      (fun __tmp1 ->
        match __tmp1 with
        | Work_node.Goal (Goal.BuildPackage build) -> is_kernel_build build
        | _ -> false)
  in
  let* artifact =
    require_completed_node
      summary
      "PackageArtifact"
      (fun __tmp1 ->
        match __tmp1 with
        | Work_node.PackageArtifact build -> is_kernel_build build
        | _ -> false)
  in
  let* archive =
    require_completed_node
      summary
      "OCamlArchive"
      (fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlArchive build -> is_kernel_build build
        | _ -> false)
  in
  let* module_dependencies =
    require_completed_node
      summary
      "ModuleDependencies"
      (fun __tmp1 ->
        match __tmp1 with
        | Work_node.ModuleDependencies build -> is_kernel_build build
        | _ -> false)
  in
  let* interface =
    require_completed_node
      summary
      "OCamlInterface"
      (fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlInterface source -> is_kernel_build source.Rule.build
        | _ -> false)
  in
  let* byte_implementation =
    require_completed_node
      summary
      "OCamlByteImplementation"
      (fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlByteImplementation source -> is_kernel_build source.Rule.build
        | _ -> false)
  in
  let* implementation =
    require_completed_node
      summary
      "OCamlImplementation"
      (fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlImplementation source -> is_kernel_build source.Rule.build
        | _ -> false)
  in
  let* generated =
    require_completed_node
      summary
      "OCamlGenerated"
      (fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlGenerated source -> is_kernel_build source.Rule.build
        | _ -> false)
  in
  let* c_object =
    require_completed_node
      summary
      "CObject"
      (fun __tmp1 ->
        match __tmp1 with
        | Work_node.CObject c_object -> is_kernel_build c_object.Rule.build
        | _ -> false)
  in
  let* _toolchain =
    require_completed_node
      summary
      "ToolchainReady"
      (fun __tmp1 ->
        match __tmp1 with
        | Work_node.ToolchainReady toolchain ->
            Riot_model.Target.equal toolchain.target (current_target ())
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"UserIntent"
      ~from:intent
      ~to_label:"Goal.BuildPackage(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.Goal (Goal.BuildPackage build) -> is_kernel_build build
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"Goal.BuildPackage(kernel)"
      ~from:goal
      ~to_label:"PackageArtifact(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.PackageArtifact build -> is_kernel_build build
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"PackageArtifact(kernel)"
      ~from:artifact
      ~to_label:"OCamlArchive(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlArchive build -> is_kernel_build build
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"OCamlArchive(kernel)"
      ~from:archive
      ~to_label:"ToolchainReady"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ToolchainReady toolchain ->
            Riot_model.Target.equal toolchain.target (current_target ())
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"OCamlArchive(kernel)"
      ~from:archive
      ~to_label:"OCamlInterface(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlInterface source -> is_kernel_build source.Rule.build
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"OCamlArchive(kernel)"
      ~from:archive
      ~to_label:"OCamlImplementation(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlImplementation source -> is_kernel_build source.Rule.build
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"OCamlArchive(kernel)"
      ~from:archive
      ~to_label:"CObject(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.CObject c_object -> is_kernel_build c_object.Rule.build
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"ModuleDependencies(kernel)"
      ~from:module_dependencies
      ~to_label:"SourceAnalysis(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.SourceAnalysis source ->
            Riot_model.Package_name.equal source.key.package kernel_package
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"OCamlInterface(kernel)"
      ~from:interface
      ~to_label:"ModuleDependencies(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ModuleDependencies build -> is_kernel_build build
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"OCamlImplementation(kernel)"
      ~from:implementation
      ~to_label:"ModuleDependencies(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ModuleDependencies build -> is_kernel_build build
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"OCamlByteImplementation(kernel)"
      ~from:byte_implementation
      ~to_label:"ModuleDependencies(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ModuleDependencies build -> is_kernel_build build
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"CObject(kernel)"
      ~from:c_object
      ~to_label:"ModuleDependencies(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ModuleDependencies build -> is_kernel_build build
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"OCamlGenerated(kernel)"
      ~from:generated
      ~to_label:"ModuleDependencies(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ModuleDependencies build -> is_kernel_build build
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"OCamlInterface(kernel)"
      ~from:interface
      ~to_label:"ToolchainReady"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ToolchainReady toolchain ->
            Riot_model.Target.equal toolchain.target (current_target ())
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"OCamlImplementation(kernel)"
      ~from:implementation
      ~to_label:"ToolchainReady"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ToolchainReady toolchain ->
            Riot_model.Target.equal toolchain.target (current_target ())
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"OCamlByteImplementation(kernel)"
      ~from:byte_implementation
      ~to_label:"ToolchainReady"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ToolchainReady toolchain ->
            Riot_model.Target.equal toolchain.target (current_target ())
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"OCamlGenerated(kernel)"
      ~from:generated
      ~to_label:"ToolchainReady"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ToolchainReady toolchain ->
            Riot_model.Target.equal toolchain.target (current_target ())
        | _ -> false)
  in
  expect_kernel_exact_cold_graph_counts summary

let expect_kernel_package_result = fun result ->
  match Build_result.package_results result
  |> List.find
    ~fn:(fun package_result ->
      Riot_model.Package_name.equal
        package_result.Build_result.package
        kernel_package) with
  | None when Build_result.has_failures result ->
      Error ("expected kernel package result; graph failed:\n"
      ^ summary_errors result.Build_result.summary)
  | None ->
      Error ("expected kernel package result; package result count="
      ^ Int.to_string (List.length (Build_result.package_results result))
      ^ "; completed kinds: "
      ^ completed_kind_names result.Build_result.summary)
  | Some package_result ->
      match package_result.Build_result.status with
      | Build_result.Built _
      | Cached _ -> Ok ()
      | Failed error -> Error ("kernel package failed: " ^ Error.message error)

let expect_cached_kernel_package_result = fun result ->
  match Build_result.package_results result
  |> List.find
    ~fn:(fun package_result ->
      Riot_model.Package_name.equal
        package_result.Build_result.package
        kernel_package) with
  | None when Build_result.has_failures result ->
      Error ("expected kernel package result; graph failed:\n"
      ^ summary_errors result.Build_result.summary)
  | None ->
      Error ("expected kernel package result; package result count="
      ^ Int.to_string (List.length (Build_result.package_results result))
      ^ "; completed kinds: "
      ^ completed_kind_names result.Build_result.summary)
  | Some package_result ->
      match package_result.Build_result.status with
      | Build_result.Cached _ -> Ok ()
      | Built _ -> Error "expected repeated kernel build to return a package cache hit"
      | Failed error -> Error ("kernel package failed: " ^ Error.message error)

let expect_kernel_work_graph = fun result ->
  if Build_result.has_failures result then
    Error ("kernel build graph failed:\n" ^ summary_errors result.Build_result.summary)
  else
    let summary = result.Build_result.summary in
    let* () = expect_kernel_package_result result in
    let* () = expect_kernel_exact_cold_graph_counts summary in
    let* () =
      expect_completed_kind
        summary
        "UserIntent"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.UserIntent _ -> true
          | _ -> false)
    in
    let* () =
      expect_completed_kind
        summary
        "Goal"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.Goal (Goal.BuildPackage { package; _ }) ->
              Riot_model.Package_name.equal package kernel_package
          | _ -> false)
    in
    let* () =
      expect_completed_kind
        summary
        "PackageArtifact"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.PackageArtifact build ->
              Riot_model.Package_name.equal build.package kernel_package
          | _ -> false)
    in
    let* () =
      expect_completed_kind
        summary
        "ToolchainReady"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.ToolchainReady toolchain ->
              Riot_model.Target.equal toolchain.target (current_target ())
          | _ -> false)
    in
    let* () =
      expect_completed_kind
        summary
        "SourceAnalysis"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.SourceAnalysis source ->
              Riot_model.Package_name.equal source.key.package kernel_package
          | _ -> false)
    in
    let* () =
      expect_completed_kind
        summary
        "ModuleDependencies"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.ModuleDependencies build -> is_kernel_build build
          | _ -> false)
    in
    let* () =
      expect_completed_kind
        summary
        "OCamlInterface"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.OCamlInterface source -> is_kernel_build source.Rule.build
          | _ -> false)
    in
    let* () =
      expect_completed_kind
        summary
        "OCamlByteImplementation"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.OCamlByteImplementation source -> is_kernel_build source.Rule.build
          | _ -> false)
    in
    let* () =
      expect_completed_kind
        summary
        "OCamlImplementation"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.OCamlImplementation source -> is_kernel_build source.Rule.build
          | _ -> false)
    in
    let* () =
      expect_completed_kind
        summary
        "OCamlGenerated"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.OCamlGenerated source -> is_kernel_build source.Rule.build
          | _ -> false)
    in
    let* () =
      expect_completed_kind
        summary
        "CObject"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.CObject c_object -> is_kernel_build c_object.Rule.build
          | _ -> false)
    in
    let* () =
      expect_completed_kind
        summary
        "OCamlArchive"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.OCamlArchive build -> is_kernel_build build
          | _ -> false)
    in
    Ok ()

let build_kernel_with_executor = fun workspace ->
  let intent =
    User_intent.build
      ~packages:(User_intent.NamedPackages [ kernel_package ])
      ~targets:(User_intent.ManyTargets [ current_target () ])
      ~profiles:(User_intent.ManyProfiles [ Riot_model.Profile.debug ])
      ()
  in
  let config = Config.make ~workspace ~parallelism:4 () in
  let* executor =
    Riot_build2.create_executor ~config ()
    |> Result.map_err ~fn:Error.message
  in
  let* result =
    Riot_build2.execute executor intent
    |> Result.map_err ~fn:Error.message
  in
  Ok (executor, result)

let build_kernel = fun workspace ->
  let* (_executor, result) = build_kernel_with_executor workspace in
  Ok result

let test_kernel_build_is_planned_and_executed = fun _ctx ->
  with_kernel_workspace_target
    (relative_target_dir "kernel-build")
    (fun workspace ->
      let intent =
        User_intent.build
          ~packages:(User_intent.NamedPackages [ kernel_package ])
          ~targets:(User_intent.ManyTargets [ current_target () ])
          ~profiles:(User_intent.ManyProfiles [ Riot_model.Profile.debug ])
          ()
      in
      let event_count = ref 0 in
      let config =
        Config.make
          ~workspace
          ~parallelism:4
          ~on_event:(fun _event -> event_count := Int.succ !event_count)
          ()
      in
      let* executor =
        Riot_build2.create_executor ~config ()
        |> Result.map_err ~fn:Error.message
      in
      let* result =
        Riot_build2.execute executor intent
        |> Result.map_err ~fn:Error.message
      in
      if Int.equal !event_count 0 then
        Error "expected build2 executor config to receive work events"
      else
        let* () = expect_kernel_work_graph result in
        let* () = expect_kernel_exact_cold_action_result_counts executor result.Build_result.summary in
        expect_kernel_graph_shape result)

let test_kernel_cold_build_graph_has_expected_edges = fun _ctx ->
  with_kernel_workspace_target
    (relative_target_dir "kernel-graph-shape")
    (fun workspace ->
      let* (executor, result) = build_kernel_with_executor workspace in
      if Build_result.has_failures result then
        Error ("kernel build graph failed:\n" ^ summary_errors result.Build_result.summary)
      else
        let* () = expect_kernel_exact_cold_action_result_counts executor result.Build_result.summary in
        expect_kernel_graph_shape result)

let test_kernel_repeated_build_uses_package_cache_fast_path = fun _ctx ->
  with_kernel_workspace_target
    (relative_target_dir "kernel-warm-cache")
    (fun workspace ->
      let* (_first_executor, first) = build_kernel_with_executor workspace in
      let* () = expect_kernel_work_graph first in
      let* (second_executor, second) = build_kernel_with_executor workspace in
      if Build_result.has_failures second then
        Error ("cached kernel build graph failed:\n" ^ summary_errors second.Build_result.summary)
      else
        let summary = second.Build_result.summary in
        let* () = expect_cached_kernel_package_result second in
        let* () = expect_kernel_exact_warm_graph_counts summary in
        expect_kernel_exact_warm_action_result_counts second_executor summary)

let test_kernel_event_change_partially_invalidates_cached_build = fun _ctx ->
  with_isolated_kernel_workspace
    (fun workspace ->
      let* (_first_executor, first) = build_kernel_with_executor workspace in
      let* () = expect_kernel_package_result first in
      let* () = expect_kernel_exact_isolated_graph_counts first.Build_result.summary in
      let* () = mutate_kernel_event_source workspace 1 in
      let* (second_executor, second) = build_kernel_with_executor workspace in
      if Build_result.has_failures second then
        Error ("partially warm kernel build graph failed:\n"
        ^ summary_errors second.Build_result.summary)
      else
        let summary = second.Build_result.summary in
        let* () = expect_kernel_package_result second in
        let* () = expect_kernel_exact_event_change_graph_counts summary in
        let* () = expect_kernel_event_change_action_result_counts second_executor summary in
        expect_cached_kernel_actions_restore_outputs_to_package_sandbox second_executor)

let test_kernel_builds_multiple_profiles_and_targets = fun _ctx ->
  with_kernel_workspace_target
    (relative_target_dir "kernel-profile-target-matrix")
    (fun workspace ->
      let* targets = build_matrix_targets workspace in
      let profiles = [
        Riot_model.Profile.debug;
        Riot_model.Profile.release;
        Riot_model.Profile.fuzz;
      ]
      in
      let intent =
        User_intent.build
          ~packages:(User_intent.NamedPackages [ kernel_package ])
          ~targets:(User_intent.ManyTargets targets)
          ~profiles:(User_intent.ManyProfiles profiles)
          ()
      in
      let config = Config.make ~workspace () in
      let* executor =
        Riot_build2.create_executor ~config ()
        |> Result.map_err ~fn:Error.message
      in
      let* result =
        Riot_build2.execute executor intent
        |> Result.map_err ~fn:Error.message
      in
      if Build_result.has_failures result then
        Error ("kernel profile/target matrix build failed:\n" ^ summary_errors result.summary)
      else
        let* () = expect_kernel_build_matrix_results result ~profiles ~targets in
        let expected_action_count = 254 * List.length profiles * List.length targets in
        let actual_action_count = List.length (kernel_action_results executor) in
        if Int.equal actual_action_count expected_action_count then
          Ok ()
        else
          Error ("expected kernel matrix build to record "
          ^ Int.to_string expected_action_count
          ^ " action results, got "
          ^ Int.to_string actual_action_count))

let tests =
  Test.[
    case
      "kernel build intent plans concrete package goal"
      test_kernel_intent_plans_concrete_package_goal;
    case
      ~size:Large
      "kernel build is planned and executed"
      test_kernel_build_is_planned_and_executed;
    case
      ~size:Large
      "kernel cold build graph has expected edges"
      test_kernel_cold_build_graph_has_expected_edges;
    case
      ~size:Large
      "kernel repeated build uses package cache fast path"
      test_kernel_repeated_build_uses_package_cache_fast_path;
    case
      ~size:Large
      "kernel async event source change partially invalidates cached build"
      test_kernel_event_change_partially_invalidates_cached_build;
    case
      ~size:Large
      "kernel builds multiple profiles and targets"
      test_kernel_builds_multiple_profiles_and_targets;
  ]

let main ~args = Test.Cli.main ~name:"riot_build2_kernel_build_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
