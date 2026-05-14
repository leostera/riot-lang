open Std
open Std.Collections
open Std.Result.Syntax

type build_scope = Riot_build.Request.scope =
  | Runtime
  | Dev
  | Dependencies

type dev_artifacts = Riot_build.Request.dev_artifacts = {
  tests: bool;
  examples: bool;
  benches: bool;
}

type request = {
  workspace: Riot_model.Workspace.t;
  packages: Riot_model.Package_name.t list;
  targets: Riot_model.Target.request;
  scope: build_scope;
  dev_artifacts: dev_artifacts;
  profile: Riot_model.Profile.t;
  requested_parallelism: int option;
  mode: Ui.mode;
  show_finished_summary: bool;
  include_external_packages: bool;
}

let build_request_label = fun (request: request) ->
  let packages =
    match request.packages with
    | [] -> "all"
    | packages ->
        packages
        |> List.map ~fn:Riot_model.Package_name.to_string
        |> String.concat ","
  in
  let targets =
    match request.targets with
    | Riot_model.Target.Host -> "host"
    | Riot_model.Target.All -> "all"
    | Riot_model.Target.Pattern pattern -> pattern
    | Riot_model.Target.Exact targets ->
        Riot_model.Target.Set.to_list targets
        |> List.map ~fn:Riot_model.Target.to_string
        |> String.concat ","
  in
  "Build(" ^ packages ^ "; targets=" ^ targets ^ "; profile=" ^ request.profile.name ^ ")"

let build_args =
  let open ArgParser in
  let open ArgParser.Arg in
  [
    option "package"
    |> short 'p'
    |> long "package"
    |> multiple
    |> help
      "Build a specific package. Repeat to build multiple packages; omit to build all packages.";
    option "target"
    |> short 'x'
    |> long "target"
    |> help "Target architecture (exact triple, pattern like 'linux'/'aarch64', or 'all')";
    flag "all-targets"
    |> long "all-targets"
    |> help "Build for all configured targets";
    flag "tests"
    |> long "tests"
    |> help "Also compile test binaries";
    flag "examples"
    |> long "examples"
    |> help "Also compile example binaries";
    flag "benches"
    |> long "benches"
    |> help "Also compile benchmark binaries";
    flag "all"
    |> long "all"
    |> help "Also compile tests, examples, and benchmark binaries";
    flag "deps"
    |> long "deps"
    |> help
      "Fetch and build third-party dependencies from riot.lock without building workspace members";
    flag "release"
    |> long "release"
    |> help "Use the release build profile";
    option "jobs"
    |> short 'j'
    |> long "jobs"
    |> help "Limit parallel workers";
    option "target-dir"
    |> long "target-dir"
    |> help "Override the workspace target directory for this command";
    flag "json"
    |> long "json"
    |> help "Emit machine-readable JSONL events";
  ]

let watch_arg =
  let open ArgParser.Arg in
  flag "watch"
  |> short 'w'
  |> long "watch"
  |> help "Watch selected workspace packages and rebuild when files change"

let command =
  let open ArgParser in
  command "build"
  |> about "Build packages"
  |> args (build_args @ [ watch_arg ])

let target_request_of_matches = fun matches ->
  if ArgParser.get_flag matches "all-targets" then
    Riot_model.Target.All
  else
    match ArgParser.get_one matches "target" with
    | Some value -> Riot_model.Target.parse value
    | None -> Riot_model.Target.Host

let mode_of_matches = fun matches -> Ui.mode_of_json_flag (ArgParser.get_flag matches "json")

let watch_of_matches = fun matches -> ArgParser.get_flag matches "watch"

let deps_of_matches = fun matches -> ArgParser.get_flag matches "deps"

let dev_artifacts_of_matches = fun matches ->
  if ArgParser.get_flag matches "all" then
    { tests = true; examples = true; benches = true }
  else
    {
      tests = ArgParser.get_flag matches "tests";
      examples = ArgParser.get_flag matches "examples";
      benches = ArgParser.get_flag matches "benches";
    }

let scope_of_dev_artifacts = fun (dev_artifacts: dev_artifacts) ->
  if dev_artifacts.tests || dev_artifacts.examples || dev_artifacts.benches then
    Dev
  else
    Runtime

let profile_of_matches = fun matches ->
  if ArgParser.get_flag matches "release" then
    Riot_model.Profile.release
  else
    Riot_model.Profile.debug

let requested_parallelism_of_matches = fun matches ->
  match ArgParser.get_one matches "jobs" with
  | None -> Ok None
  | Some value -> (
      match Int.parse value with
      | Some workers -> Ok (Some workers)
      | None -> Error (Failure ("invalid --jobs value: " ^ value))
    )

let parse_package_names = fun package_names ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | package_name :: rest -> (
        match Riot_model.Package_name.from_string package_name with
        | Ok package_name -> loop (package_name :: acc) rest
        | Error error ->
            Error (Failure ("invalid package name '"
            ^ package_name
            ^ "': "
            ^ Riot_model.Package_name.error_message error))
      )
  in
  loop [] package_names

let make_request = fun
  ~workspace
  ?(scope = Runtime)
  ?(dev_artifacts = {tests = true; examples = true; benches = true})
  ?(profile = Riot_model.Profile.debug)
  ?(mode = Ui.default_human_mode ())
  ?(show_finished_summary = true)
  ?(requested_parallelism = None)
  ?(include_external_packages = false)
  ~packages
  ~targets
  () ->
  {
    workspace;
    packages;
    targets;
    scope;
    dev_artifacts;
    profile;
    requested_parallelism;
    mode;
    show_finished_summary;
    include_external_packages;
  }

let request_of_matches = fun ~workspace matches ->
  match parse_package_names (ArgParser.get_many matches "package") with
  | Error _ as err -> err
  | Ok packages -> (
      match requested_parallelism_of_matches matches with
      | Error _ as err -> err
      | Ok requested_parallelism ->
          let deps_only = deps_of_matches matches in
          let dev_artifacts = dev_artifacts_of_matches matches in
          Ok (
            make_request
              ~workspace
              ~scope:(
                if deps_only then
                  Dependencies
                else
                  scope_of_dev_artifacts dev_artifacts
              )
              ~dev_artifacts
              ~profile:(profile_of_matches matches)
              ~mode:(mode_of_matches matches)
              ~requested_parallelism
              ~include_external_packages:deps_only
              ~packages
              ~targets:(target_request_of_matches matches)
              ()
          )
    )

let should_build_fix_provider_runner = fun (request: request) ->
  match request.scope with
  | Runtime -> false
  | Dependencies -> false
  | Dev ->
      request.dev_artifacts.tests && request.dev_artifacts.examples && request.dev_artifacts.benches

let workspace_fix_providers = fun (workspace: Riot_model.Workspace.t) ->
  workspace.packages
  |> List.filter ~fn:Riot_model.Package_manifest.is_workspace_member
  |> List.map ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> pkg.fix_providers)
  |> List.concat

let workspace_member_package_names = fun (workspace: Riot_model.Workspace.t) ->
  workspace.packages
  |> List.filter ~fn:Riot_model.Package_manifest.is_workspace_member
  |> List.map ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> pkg.name)

let selected_fix_providers = fun (request: request) ->
  let providers = workspace_fix_providers request.workspace in
  match request.packages with
  | [] -> providers
  | selected ->
      providers
      |> List.filter
        ~fn:(fun (provider: Riot_model.Fix_provider.t) ->
          List.any
            selected
            ~fn:(fun package_name ->
              Riot_model.Package_name.equal
                provider.package_name
                package_name))

let trace_build = fun message ->
  let _ = message in
  ()

let run_request = fun (request: request) ->
  trace_build ("run_request request=" ^ build_request_label request);
  let start_time = Time.Instant.now () in
  if request.mode = Ui.Json then
    Ui.reset_json_clock ~started_at:start_time;
  let ui = Ui.make ~mode:request.mode ~profile:request.profile.name () in
  let progress: Ui.Common.build_progress = {
    built_count = 0;
    cached_count = 0;
    failed_count = 0;
    skipped_count = 0;
  }
  in
  let attempted_build = ref false in
  let on_build_event event =
    attempted_build := true;
    Ui.Common.record_build_event_progress progress event;
    Ui.send ui event
  in
  let build_request = fun
    ?(synthetic_tools = []) ~workspace ~packages ~targets ~scope ~dev_artifacts ~profile () ->
    Riot_build.build
      ~on_event:on_build_event
      (Riot_build.Request.make
        ~workspace
        ~packages
        ~targets
        ~scope
        ~dev_artifacts
        ~profile
        ~synthetic_tools
        ~requested_parallelism:request.requested_parallelism
        ~include_external_packages:request.include_external_packages
        ())
    |> Result.map ~fn:(fun _output -> ())
  in
  let fix_provider_runner_plan = fun () ->
    let providers = selected_fix_providers request in
    if List.is_empty providers then
      None
    else
      Some (Riot_fix.Fixme_runner.materialize
        ~workspace_root:request.workspace.root
        ~target_dir_root:request.workspace.target_dir_root
        providers)
  in
  let runner_plan =
    if should_build_fix_provider_runner request then
      fix_provider_runner_plan ()
    else
      None
  in
  let build_workspace =
    match runner_plan with
    | Some plan -> Riot_fix.Fixme_runner.attach_to_workspace request.workspace plan
    | None -> request.workspace
  in
  let build_packages =
    match (runner_plan, request.packages) with
    | (Some _, []) -> workspace_member_package_names request.workspace
    | _ -> request.packages
  in
  let synthetic_tools =
    match runner_plan with
    | Some plan ->
        [ Riot_planner.Build_unit_graph.{ package = plan.package_name; name = plan.binary_name }; ]
    | None -> []
  in
  let result =
    build_request
      ~workspace:build_workspace
      ~packages:build_packages
      ~targets:request.targets
      ~scope:request.scope
      ~dev_artifacts:request.dev_artifacts
      ~profile:request.profile
      ~synthetic_tools
      ()
    |> Result.map_err
      ~fn:(fun err ->
        Ui.clear ui;
        Ui.send_build_error ui err;
        Failure (Riot_build.error_message err))
  in
  if request.show_finished_summary && !attempted_build then (
    let duration = Time.Instant.duration_since ~earlier:start_time (Time.Instant.now ()) in
    Ui.send_build_finished ui ~duration ~progress
  );
  result

let print_workspace_load_errors = fun errors ->
  let ui = Ui.make ~mode:Ui.Line () in
  List.for_each
    errors
    ~fn:(fun err ->
      Ui.send_command_error
        ui
        ~kind:"WorkspaceLoadError"
        ~details:[
          ("message", Data.Json.String (Riot_model.Workspace_manager.load_error_to_string err));
        ]
        ~message:(Riot_model.Workspace_manager.load_error_to_string err))

type loaded_workspace = {
  workspace: Riot_model.Workspace.t;
}

let load_workspace_strict = fun cwd ->
  let workspace_manager = Riot_model.Workspace_manager.create () in
  let* registry =
    Pkgs_ml.Registry.create_filesystem ?riot_home:None ~registry_name:"pkgs.ml" ()
    |> Result.map_err ~fn:(fun err -> Failure (Pkgs_ml.Registry_cache.create_error_message err))
  in
  match Riot_model.Workspace_manager.scan workspace_manager cwd with
  | Error err -> Error (Failure (Riot_model.Workspace_manager.scan_error_message err))
  | Ok (_workspace, load_errors) when List.length load_errors > 0 ->
      print_workspace_load_errors load_errors;
      Error (Failure "Workspace load failed")
  | Ok (workspace, _) ->
      let* workspace =
        Riot_deps.ensure_workspace
          ~workspace_manager
          ~mode:Riot_deps.Dep_solver.Refresh
          ~registry
          ~workspace
          ()
        |> Result.map_err ~fn:(fun err -> Failure (Riot_model.Pm_error.message err))
      in
      Ok { workspace }

let profile_of_string = fun __tmp1 ->
  match __tmp1 with
  | "release" -> Riot_model.Profile.release
  | "fuzz" -> Riot_model.Profile.fuzz
  | _ -> Riot_model.Profile.debug

let build_command = fun
  ~workspace
  ?(scope = Runtime)
  ?(dev_artifacts = {tests = true; examples = true; benches = true})
  ?(profile = "debug")
  ?(mode = Ui.default_human_mode ())
  ?(show_finished_summary = true)
  ?(requested_parallelism = None)
  package_opt
  target_arch ->
  let packages = Option.to_list package_opt in
  run_request
    (
      make_request
        ~workspace
        ~scope
        ~dev_artifacts
        ~profile:(profile_of_string profile)
        ~mode
        ~show_finished_summary
        ~requested_parallelism
        ~packages
        ~targets:(
          match target_arch with
          | Some target -> Riot_model.Target.parse target
          | None -> Riot_model.Target.Host
        )
        ()
    )

let run = fun ~workspace matches ->
  match request_of_matches ~workspace matches with
  | Error _ as err -> err
  | Ok request ->
      if watch_of_matches matches then
        match request.scope with
        | Dependencies -> Error (Failure "--watch is not supported with --deps")
        | Runtime
        | Dev ->
            Watch.run
              ~command:"build"
              ~workspace
              ~package_filters:request.packages
              ~mode:request.mode
              ~run_once:(fun () -> run_request request)
              ()
      else
        run_request request
