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

let expect_kernel_build_work = fun actual ->
  match actual with
  | [ Package_work.BuildLibrary { package; scope; profile; target } ] when Riot_model.Package_name.equal
    package
    kernel_package
  && scope = Package_work.Runtime
  && profile = Riot_model.Profile.debug
  && Riot_model.Target.equal target (current_target ()) -> Ok ()
  | _ -> Error "expected kernel build goal to expand to one kernel BuildLibrary work item"

let test_kernel_goal_plans_package_work = fun _ctx ->
  with_kernel_workspace
    (fun workspace ->
      let catalog = Package_catalog.create workspace in
      Goal.BuildPackage {
        package = Goal.Package kernel_package;
        profile = Riot_model.Profile.debug;
        target = current_target ();
      }
      |> Goal_planner.expand catalog
      |> Result.map_err ~fn:Error.message
      |> Result.and_then ~fn:expect_kernel_build_work)

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
  | PackageWork _ -> "PackageWork"
  | ToolchainReady _ -> "ToolchainReady"
  | SourceAnalysis _ -> "SourceAnalysis"
  | ModulePlan _ -> "ModulePlan"
  | PackageFinalize _ -> "PackageFinalize"
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
          | Work_node.Goal (Goal.BuildPackage { package = Goal.Package package; _ }) ->
              Riot_model.Package_name.equal package kernel_package
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
        "ActionExecution"
        (fun __tmp1 ->
          match __tmp1 with
          | Work_node.ActionExecution action ->
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
        expect_kernel_work_graph result)

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
    case "kernel build goal plans package work" test_kernel_goal_plans_package_work;
    case
      ~size:Large
      "kernel build is planned and executed"
      test_kernel_build_is_planned_and_executed;
    case
      ~size:Large
      "kernel repeated build uses package cache fast path"
      test_kernel_repeated_build_uses_package_cache_fast_path;
  ]

let main ~args = Test.Cli.main ~name:"riot_build2_kernel_build_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
