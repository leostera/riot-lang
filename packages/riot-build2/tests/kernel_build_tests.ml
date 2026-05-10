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

let relative_target_dir = fun name ->
  Path.(Path.v "_build"
  / Path.v "riot-build2-tests"
  / Path.v (name ^ "-" ^ Riot_model.Session_id.to_string (Riot_model.Session_id.make ())))

let current_target = fun () -> Riot_model.Target.current

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

let kernel_compiler_actions = fun summary ->
  summary.Executor.Summary.results
  |> List.filter_map
    ~fn:(fun result ->
      if result.Executor.Summary.status = Work_node.Completed then
        match Work_node.kind result.node with
        | Work_node.OCamlLibrary action
        | Work_node.OCamlArchive action
        | Work_node.ActionExecution action when Riot_model.Package_name.equal
          action.ref_.package
          kernel_package -> Some action
        | _ -> None
      else
        None)

let expect_kernel_native_compile_library = fun summary ->
  let actions = kernel_compiler_actions summary in
  let compile_library_source_actions =
    actions
    |> List.filter
      ~fn:(fun action ->
        match action.Action_execution.action with
        | Action.CompileSource _ -> true
        | Action.CompileSources { sources; _ } -> not (List.is_empty sources)
        | _ -> false)
  in
  let grouped_compile_library_count =
    actions
    |> List.filter
      ~fn:(fun action ->
        match action.Action_execution.action with
        | Action.CompileSources { sources; _ } -> List.length sources > 1
        | _ -> false)
    |> List.length
  in
  let concrete_grouped_compile_library_count =
    actions
    |> List.filter
      ~fn:(fun action ->
        match action.Action_execution.action with
        | Action.CompileSources { sources; _ } ->
            List.length sources > 1
            && not (List.any sources ~fn:(fun source -> Option.is_some source.Action.content))
        | _ -> false)
    |> List.length
  in
  let final_archive_count =
    actions
    |> List.filter
      ~fn:(fun action ->
        match action.Action_execution.action with
        | Action.CompileLibrary { sources = []; outputs; _ } ->
            let has_cmxa =
              List.any outputs ~fn:(fun output -> Path.extension output = Some ".cmxa")
            in
            let has_a =
              List.any outputs ~fn:(fun output -> Path.extension output = Some ".a")
            in
            has_cmxa && has_a
        | _ -> false)
    |> List.length
  in
  if
    List.length compile_library_source_actions > 1
    && grouped_compile_library_count > 0
    && concrete_grouped_compile_library_count > 0
    && Int.equal final_archive_count 1
  then
    Ok ()
  else
    Error ("expected kernel to execute grouped native CompileSources actions and one final archive, got "
    ^ Int.to_string (List.length compile_library_source_actions)
    ^ " source actions, "
    ^ Int.to_string grouped_compile_library_count
    ^ " grouped source actions, and "
    ^ Int.to_string concrete_grouped_compile_library_count
    ^ " concrete grouped source actions, and "
    ^ Int.to_string final_archive_count
    ^ " final archive actions out of "
    ^ Int.to_string (List.length actions)
    ^ " kernel actions")

let expect_kernel_graph_shape = fun result ->
  let summary = result.Build_result.summary in
  let is_kernel_build = fun (build: Goal.build_package) ->
    Riot_model.Package_name.equal
      build.package
      kernel_package
  in
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
  let* finalize =
    require_completed_node
      summary
      "PackageFinalize"
      (fun __tmp1 ->
        match __tmp1 with
        | Work_node.PackageFinalize build -> is_kernel_build build
        | _ -> false)
  in
  let* action_plan =
    require_completed_node
      summary
      "ActionPlan"
      (fun __tmp1 ->
        match __tmp1 with
        | Work_node.ActionPlan build -> is_kernel_build build
        | _ -> false)
  in
  let* module_plan =
    require_completed_node
      summary
      "ModulePlan"
      (fun __tmp1 ->
        match __tmp1 with
        | Work_node.ModulePlan build -> is_kernel_build build
        | _ -> false)
  in
  let* archive =
    require_completed_node
      summary
      "OCamlArchive"
      (fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlArchive action ->
            Riot_model.Package_name.equal action.ref_.package kernel_package
        | _ -> false)
  in
  let* library =
    require_completed_node
      summary
      "OCamlLibrary"
      (fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlLibrary action ->
            Riot_model.Package_name.equal action.ref_.package kernel_package
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
      ~to_label:"PackageFinalize(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.PackageFinalize build -> is_kernel_build build
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"PackageFinalize(kernel)"
      ~from:finalize
      ~to_label:"ActionPlan(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ActionPlan build -> is_kernel_build build
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"ActionPlan(kernel)"
      ~from:action_plan
      ~to_label:"ModulePlan(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.ModulePlan build -> is_kernel_build build
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"ModulePlan(kernel)"
      ~from:module_plan
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
      ~from_label:"PackageFinalize(kernel)"
      ~from:finalize
      ~to_label:"OCamlArchive(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlArchive action ->
            Riot_model.Package_name.equal action.ref_.package kernel_package
        | _ -> false)
  in
  let* () =
    expect_dependency
      summary
      ~from_label:"OCamlArchive(kernel)"
      ~from:archive
      ~to_label:"OCamlLibrary(kernel)"
      ~dependency:(fun __tmp1 ->
        match __tmp1 with
        | Work_node.OCamlLibrary action ->
            Riot_model.Package_name.equal action.ref_.package kernel_package
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
  let* () = expect_kernel_native_compile_library summary in
  if node_depends_on
    summary
    library
    ~dependency:(fun __tmp1 ->
      match __tmp1 with
      | Work_node.ToolchainReady toolchain ->
          Riot_model.Target.equal toolchain.target (current_target ())
      | _ -> false) then
    Ok ()
  else
    Error "expected at least one kernel OCaml library to depend on current toolchain readiness"

let expect_kernel_package_result = fun result ->
  match Build_result.package_results result
  |> List.find
    ~fn:(fun package_result ->
      Riot_model.Package_name.equal
        package_result.Build_result.package
        kernel_package) with
  | None -> Error "expected kernel package result"
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
  | None -> Error "expected kernel package result"
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
    let* () = expect_kernel_native_compile_library summary in
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
        "PackageFinalize"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.PackageFinalize build ->
              Riot_model.Package_name.equal build.package kernel_package
          | _ -> false)
    in
    let* () =
      expect_completed_kind
        summary
        "ActionPlan"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.ActionPlan build -> Riot_model.Package_name.equal build.package kernel_package
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
        "ModulePlan"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.ModulePlan build -> Riot_model.Package_name.equal build.package kernel_package
          | _ -> false)
    in
    let* () =
      expect_completed_kind
        summary
        "OCamlLibrary"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.OCamlLibrary action ->
              Riot_model.Package_name.equal action.ref_.package kernel_package
          | _ -> false)
    in
    let* () =
      expect_completed_kind
        summary
        "OCamlArchive"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.OCamlArchive action ->
              Riot_model.Package_name.equal action.ref_.package kernel_package
          | _ -> false)
    in
    Ok ()

let expect_no_completed_kind = fun summary label fn ->
  if completed_kind summary ~fn then
    Error ("expected cached build to skip " ^ label ^ " nodes")
  else
    Ok ()

let build_kernel = fun workspace ->
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
  Riot_build2.execute executor intent
  |> Result.map_err ~fn:Error.message

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
        expect_kernel_graph_shape result)

let test_kernel_cold_build_graph_has_expected_edges = fun _ctx ->
  with_kernel_workspace_target
    (relative_target_dir "kernel-graph-shape")
    (fun workspace ->
      let* result = build_kernel workspace in
      if Build_result.has_failures result then
        Error ("kernel build graph failed:\n" ^ summary_errors result.Build_result.summary)
      else
        expect_kernel_graph_shape result)

let test_kernel_repeated_build_uses_package_cache_fast_path = fun _ctx ->
  with_kernel_workspace_target
    (relative_target_dir "kernel-warm-cache")
    (fun workspace ->
      let* first = build_kernel workspace in
      let* () = expect_kernel_work_graph first in
      let* second = build_kernel workspace in
      if Build_result.has_failures second then
        Error ("cached kernel build graph failed:\n" ^ summary_errors second.Build_result.summary)
      else
        let summary = second.Build_result.summary in
        let* () = expect_cached_kernel_package_result second in
        let* () =
          expect_no_completed_kind
            summary
            "SourceAnalysis"
            (fun __tmp1 ->
              match __tmp1 with
              | Work_node.SourceAnalysis source ->
                  Riot_model.Package_name.equal source.key.package kernel_package
              | _ -> false)
        in
        let* () =
          expect_no_completed_kind
            summary
            "ModulePlan"
            (fun __tmp1 ->
              match __tmp1 with
              | Work_node.ModulePlan build ->
                  Riot_model.Package_name.equal build.package kernel_package
              | _ -> false)
        in
        let* () =
          expect_no_completed_kind
            summary
            "OCamlLibrary"
            (fun __tmp1 ->
              match __tmp1 with
              | Work_node.OCamlLibrary action ->
                  Riot_model.Package_name.equal action.ref_.package kernel_package
              | _ -> false)
        in
        let* () =
          expect_no_completed_kind
            summary
            "OCamlArchive"
            (fun __tmp1 ->
              match __tmp1 with
              | Work_node.OCamlArchive action ->
                  Riot_model.Package_name.equal action.ref_.package kernel_package
              | _ -> false)
        in
        expect_no_completed_kind
          summary
          "ActionExecution"
          (fun __tmp1 ->
            match __tmp1 with
            | Work_node.ActionExecution action ->
                Riot_model.Package_name.equal action.ref_.package kernel_package
            | _ -> false))

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
  ]

let main ~args = Test.Cli.main ~name:"riot_build2_kernel_build_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
