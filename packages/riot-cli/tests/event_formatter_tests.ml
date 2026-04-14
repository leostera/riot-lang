open Std
open Std.Collections
module Test = Std.Test

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let test_workspace_completed_is_silent = fun _ctx ->
  let displayed_packages = HashSet.create () in
  let event = Riot_executor.Telemetry_events.WorkspaceCompleted {
    session_id = Riot_model.Session_id.make ();
    target = Riot_planner.Workspace_planner.All;
    total_duration = Time.Duration.from_millis 42;
    cached_count = 1;
    built_count = 2;
    failed_count = 0;
  }
  in
  let rendered = Riot_cli.Event_formatter.format ~displayed_packages event in
  if String.equal rendered "" then
    Ok ()
  else
    Error ("expected empty workspace summary, got: " ^ rendered)

let test_build_failed_prefixes_package_name = fun _ctx ->
  let displayed_packages = HashSet.create () in
  let package =
    Riot_model.Package.make ~name:(package_name "syn") ~path:(Path.v ".") ~relative_path:(Path.v ".") ()
  in
  let event = Riot_executor.Telemetry_events.BuildFailed {
    session_id = Riot_model.Session_id.make ();
    package;
    target = Riot_planner.Workspace_planner.Package package.name;
    error = Riot_executor.Telemetry_events.ExecutionFailed { message = "Command failed" }
  } in
  let rendered = Riot_cli.Event_formatter.format ~displayed_packages event in
  if String.contains rendered "syn: Execution failed: Command failed" then
    Ok ()
  else
    Error ("expected package-prefixed error, got: " ^ rendered)

let test_package_ocamlc_warnings_prefix_package_name = fun _ctx ->
  let displayed_packages = HashSet.create () in
  let package = Riot_model.Package.make
    ~name:(package_name "riot-eval")
    ~path:(Path.v ".")
    ~relative_path:(Path.v ".")
    () in
  let event = Riot_executor.Telemetry_events.PackageOcamlcWarnings {
    session_id = Riot_model.Session_id.make ();
    package;
    target = Riot_planner.Workspace_planner.Package package.name;
    source = `Cached;
    messages = [ "File \"x.ml\", line 1, characters 0-1:\nWarning: example" ];
  }
  in
  let rendered = Riot_cli.Event_formatter.format ~displayed_packages event in
  if String.contains rendered "riot-eval: File \"x.ml\"" then
    Ok ()
  else
    Error ("expected package-prefixed warning, got: " ^ rendered)

let tests =
  Test.[
    case "event formatter: workspace completed is silent" test_workspace_completed_is_silent;
    case "event formatter: build failed prefixes package name" test_build_failed_prefixes_package_name;
    case "event formatter: package ocamlc warnings prefix package name" test_package_ocamlc_warnings_prefix_package_name;
  ]

let name = "Riot CLI Event Formatter Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
