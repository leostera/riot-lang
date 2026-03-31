open Std
open Std.Collections
module Test = Std.Test

let test_workspace_completed_is_silent = fun () ->
    let displayed_packages = HashSet.create () in
    let event = Tusk_executor.Telemetry_events.WorkspaceCompleted {
      session_id = Tusk_model.Session_id.make ();
      target = Tusk_planner.Workspace_planner.All;
      total_duration = Time.Duration.from_millis 42;
      cached_count = 1;
      built_count = 2;
      failed_count = 0;

    } in
    let rendered = Tusk_cli.Event_formatter.format ~displayed_packages event in
    if String.equal rendered "" then
      Ok ()
    else
      Error ("expected empty workspace summary, got: " ^ rendered)

let tests =
  Test.[ case "event formatter: workspace completed is silent" test_workspace_completed_is_silent;  ]

let name = "Tusk CLI Event Formatter Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
