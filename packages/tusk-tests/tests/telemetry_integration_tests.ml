open Std
open Tusk_executor

let test_telemetry_events_are_emitted =
  Test.case "telemetry events are emitted" @@ fun () ->
  let _telemetry_pid = Telemetry.Server.start () in

  let events = ref [] in
  Telemetry.attach "test-collector" (fun event -> events := event :: !events);

  Telemetry.emit
    Telemetry_events.(
      WorkspaceStarted
        { target = Tusk_planner.Workspace_planner.All; package_count = 2 });

  Telemetry.emit
    Telemetry_events.(
      BuildCompleted
        {
          package = "test-pkg";
          target = Tusk_planner.Workspace_planner.All;
          cached = false;
          duration = Time.Duration.from_millis 100;
        });

  Telemetry.stop ();

  let has_workspace_started =
    List.exists
      (fun ev ->
        match ev with Telemetry_events.WorkspaceStarted _ -> true | _ -> false)
      !events
  in

  let has_build_completed =
    List.exists
      (fun ev ->
        match ev with Telemetry_events.BuildCompleted _ -> true | _ -> false)
      !events
  in

  if has_workspace_started && has_build_completed then Ok ()
  else
    Error
      (format
         "Expected WorkspaceStarted and BuildCompleted events, got %d events"
         (List.length !events))

let tests = [ test_telemetry_events_are_emitted ]
let name = "Telemetry Integration Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args
