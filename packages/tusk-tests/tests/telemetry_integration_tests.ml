open Std
open Tusk_model
open Tusk_executor

let test_telemetry_events_are_emitted =
  Test.case "telemetry events are emitted" @@ fun () ->
  let _telemetry_pid = Telemetry.start () in

  let events = ref [] in
  Telemetry.attach "test-collector" (fun event -> events := event :: !events);

  Telemetry.emit
    Telemetry_events.(
      WorkspaceStarted
        { target = Tusk_planner.Workspace_planner.All; package_count = 2 });

  let test_package =
    {
      Package.name = "test-pkg";
      path = Path.of_string "." |> Result.expect ~msg:"invalid path";
      relative_path = Path.of_string "." |> Result.expect ~msg:"invalid path";
      dependencies = [];
      binaries = [];
      library = None;
      sources = { src = []; native = []; tests = [] };
    }
  in
  Telemetry.emit
    Telemetry_events.(
      BuildCompleted
        {
          package = test_package;
          target = Tusk_planner.Workspace_planner.All;
          status = `Fresh;
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
