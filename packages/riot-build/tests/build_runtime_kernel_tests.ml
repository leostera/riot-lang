open Std
module Test = Std.Test

let clone_workspace_with_target = fun (workspace: Riot_model.Workspace.t) ~target_dir ->
  Riot_model.Workspace.make
    ?name:workspace.name
    ~root:workspace.root
    ~packages:workspace.packages
    ~dependencies:workspace.dependencies
    ~dev_dependencies:workspace.dev_dependencies
    ~build_dependencies:workspace.build_dependencies
    ~profile_overrides:workspace.profile_overrides
    ~target_dir:(Path.to_string target_dir)
    ()

let load_repo_workspace = fun () ->
  let manager = Riot_model.Workspace_manager.create () in
  match Riot_model.Workspace_manager.scan manager (Path.v ".") with
  | Error err ->
      Error ("workspace scan failed: " ^ err)
  | Ok (workspace, errors) ->
      if List.is_empty errors then
        Ok workspace
      else
        Error ("workspace scan produced load errors: "
        ^ String.concat "; " (List.map errors ~fn:Riot_model.Workspace_manager.load_error_to_string))

let render_streaming_event = fun (event: Riot_build.Client.streaming_event) ->
  match event with
  | Riot_build.Client.BuildStarted _ ->
      "BuildStarted"
  | Riot_build.Client.BuildEvent _ ->
      "BuildEvent"
  | Riot_build.Client.BuildCompleted _ ->
      "BuildCompleted"
  | Riot_build.Client.BuildFailed _ ->
      "BuildFailed"
  | Riot_build.Client.PlanningFailed _ ->
      "PlanningFailed"
  | Riot_build.Client.CycleDetected _ ->
      "CycleDetected"

let render_build_event = fun (event: Riot_build.build_event) ->
  match event with
  | Riot_build.Pm event ->
      "Pm(" ^ Riot_model.Event.name event.kind ^ ")"
  | Riot_build.BuildingTarget { target; host } ->
      "BuildingTarget(" ^ Riot_model.Target.to_string target ^ "," ^ Bool.to_string host ^ ")"
  | Riot_build.CacheGc _ ->
      "CacheGc"
  | Riot_build.Phase _ ->
      "Phase"
  | Riot_build.Streaming event ->
      "Streaming(" ^ render_streaming_event event ^ ")"

let has_workspace_plan_started = fun events ->
  List.find events ~fn:(function
    | Riot_build.Streaming (Riot_build.Client.BuildEvent (Riot_executor.Telemetry_events.WorkspacePlanStarted _)) ->
        true
    | _ ->
        false)
  |> Option.is_some

let has_workspace_plan_completed = fun events ->
  List.find events ~fn:(function
    | Riot_build.Streaming (Riot_build.Client.BuildEvent (Riot_executor.Telemetry_events.WorkspacePlanCompleted _)) ->
        true
    | _ ->
        false)
  |> Option.is_some

let summarize_build_failure = fun (err: Riot_build.build_error) events ->
  let recent_events =
    List.reverse events
    |> List.take ~len:12
    |> List.reverse
    |> List.map ~fn:render_build_event
    |> String.concat " -> "
  in
  match err with
  | Riot_build.ClientError (Riot_build.Client.BuildFailed { errors }) ->
      let rendered_errors =
        List.map errors ~fn:(fun (result: Riot_executor.Package_builder.build_result) ->
          match result.status with
          | Riot_executor.Package_builder.Failed err ->
              Riot_model.Package.key_to_string result.package_key
              ^ ": "
              ^ Riot_executor.Package_builder.package_error_to_string err
          | Riot_executor.Package_builder.Skipped { reason } ->
              Riot_model.Package.key_to_string result.package_key ^ ": skipped(" ^ reason ^ ")"
          | Riot_executor.Package_builder.Cached _
          | Riot_executor.Package_builder.Built _ ->
              Riot_model.Package.key_to_string result.package_key ^ ": unexpected-success")
        |> String.concat "\n"
      in
      "kernel build failed through Riot_build.build\n"
      ^ rendered_errors
      ^ "\nrecent events: "
      ^ recent_events
  | _ ->
      Riot_build.build_error_message err ^ "\nrecent events: " ^ recent_events

let test_build_runtime_builds_repo_kernel = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"riot_build_kernel_runtime"
      (fun tempdir ->
        match load_repo_workspace () with
        | Error _ as err ->
            err
        | Ok repo_workspace ->
            let workspace =
              clone_workspace_with_target
                repo_workspace
                ~target_dir:Path.(tempdir / Path.v "target")
            in
            let events = ref [] in
            match
              Riot_build.build
                ~on_event:(fun event -> events := event :: !events)
                {
                  workspace;
                  packages = [ "kernel" ];
                  targets = Riot_build.Host;
                  scope = Riot_build.Runtime;
                  profile = "debug";
                }
            with
            | Error err ->
                Error (summarize_build_failure err !events)
            | Ok results -> (
                match
                  List.find results ~fn:(fun (result: Riot_executor.Package_builder.build_result) ->
                    String.equal result.package.name "kernel")
                with
                | None ->
                    Error "expected kernel build result"
                | Some result -> (
                    match result.status with
                    | Riot_executor.Package_builder.Built _
                    | Riot_executor.Package_builder.Cached _ ->
                        if not (has_workspace_plan_started !events) then
                          Error "expected workspace plan started event"
                        else if not (has_workspace_plan_completed !events) then
                          Error "expected workspace plan completed event"
                        else
                          Ok ()
                    | Riot_executor.Package_builder.Skipped { reason } ->
                        Error ("expected kernel build to run, got skipped: " ^ reason)
                    | Riot_executor.Package_builder.Failed err ->
                        Error ("kernel build failed: "
                        ^ Riot_executor.Package_builder.package_error_to_string err)
                  )
              ))
  with
  | Ok result ->
      result
  | Error err ->
      Error ("tempdir failed: " ^ IO.error_message err)

let tests =
  let open Test in [
    case ~size:Large "build runtime: repo kernel builds successfully" test_build_runtime_builds_repo_kernel;
  ]

let name = "Riot Build Runtime Kernel Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
