open Std
open Std.Collections

module Common = Common
module Json = Json
module Line = Line
module Tui = Tui

type mode =
  | Json
  | TUI
  | Line

type event = Riot_model.Event.t

type renderer =
  | Json_renderer
  | Line_renderer
  | Tui_renderer of Tui.t

type state = {
  renderer: renderer;
  seen_registry_updates: string HashSet.t;
  render_state: Common.render_state;
  profile: string option;
  workspace_root: Path.t option;
}

type t = {
  pid: Pid.t;
}

let is_interactive_stderr = fun () -> Tty.is_tty (Tty.stderr_fd ())

let default_human_mode = fun () ->
  if is_interactive_stderr () then
    TUI
  else
    Line

let mode_of_json_flag = fun json ->
  if json then
    Json
  else
    default_human_mode ()

type request =
  | Send of event
  | Clear
  | Build_error of Riot_build.error
  | Command_error of {
      kind: string;
      details: (string * Data.Json.t) list;
      message: string;
    }
  | Build_finished of {
      duration: Time.Duration.t;
      progress: Common.build_progress;
    }

type request_envelope = {
  request: request;
  reply_to: Pid.t;
  request_id: int;
}

type Message.t +=
  | Riot_cli_ui_request of request_envelope
  | Riot_cli_ui_response of {
      request_id: int;
      result: (unit, exn) result;
    }

let request_ids = Sync.Atomic.make 0

let next_request_id = fun () -> Sync.Atomic.fetch_and_add request_ids 1 + 1

let make_state = fun ?profile ?workspace_root ~mode () ->
  let renderer =
    match mode with
    | Json -> Json_renderer
    | Line -> Line_renderer
    | TUI -> Tui_renderer (Tui.create ?profile ())
  in
  {
    renderer;
    seen_registry_updates = HashSet.create ();
    render_state = Common.create_render_state ?profile ();
    profile;
    workspace_root;
  }

let clear_state = fun ui ->
  match ui.renderer with
  | Json_renderer
  | Line_renderer -> ()
  | Tui_renderer dashboard -> Tui.clear dashboard

let update_render_state = fun ui event ->
  match event.Riot_model.Event.kind with
  | Riot_model.Event.Build (
    Riot_model.Event.BuildPhase (
      Riot_model.Event.TargetsResolved { target_count }
    )
  ) ->
      ui.render_state.target_count <- Some target_count
  | _ -> ()

let send_state = fun ui event ->
  update_render_state ui event;
  match ui.renderer with
  | Json_renderer -> Json.write_event event
  | Line_renderer ->
      Line.write_event
        ~render_state:ui.render_state
        ?profile:ui.profile
        ?workspace_root:ui.workspace_root
        ~seen_registry_updates:ui.seen_registry_updates
        event
  | Tui_renderer dashboard ->
      Tui.write_event
        ~render_state:ui.render_state
        ?profile:ui.profile
        ?workspace_root:ui.workspace_root
        ~seen_registry_updates:ui.seen_registry_updates
        dashboard
        event

let build_error_json = fun err ->
  match err with
  | Riot_build.TargetSelectionFailed { pattern; available_targets } -> (
    "NoTargetsMatched",
    [
      ("pattern", Data.Json.String pattern);
      (
        "available_targets",
        Data.Json.Array (List.map
          available_targets
          ~fn:(fun target -> Data.Json.String (Riot_model.Target.to_string target)))
      );
    ]
  )
  | Riot_build.PackageNotFound { package_name; available_packages } -> (
    "PackageNotFound",
    [
      ("package_name", Data.Json.String (Riot_model.Package_name.to_string package_name));
      (
        "available_packages",
        Data.Json.Array (List.map
          available_packages
          ~fn:(fun pkg -> Data.Json.String (Riot_model.Package_name.to_string pkg)))
      );
    ]
  )
  | Riot_build.PackagesNotFound { package_names; available_packages } -> (
    "PackagesNotFound",
    [
      (
        "package_names",
        Data.Json.Array (List.map
          package_names
          ~fn:(fun pkg -> Data.Json.String (Riot_model.Package_name.to_string pkg)))
      );
      (
        "available_packages",
        Data.Json.Array (List.map
          available_packages
          ~fn:(fun pkg -> Data.Json.String (Riot_model.Package_name.to_string pkg)))
      );
    ]
  )
  | Riot_build.ToolchainInstallFailed { target; error } -> (
    "ToolchainInstallFailed",
    [
      ("target", Data.Json.String (Riot_model.Target.to_string target));
      ("reason", Data.Json.String (Riot_build.toolchain_install_error_message error));
    ]
  )
  | Riot_build.ToolchainInitializationFailed { target; error } -> (
    "ToolchainInitializationFailed",
    [
      ("target", Data.Json.String (Riot_model.Target.to_string target));
      ("reason", Data.Json.String (Riot_build.toolchain_initialization_error_message error));
    ]
  )
  | Riot_build.BuildFailed { errors } -> (
    "BuildFailed",
    [ ("errors", Data.Json.Array (List.map errors ~fn:Riot_build.Build_result.failure_to_json)); ]
  )
  | Riot_build.BuildUnitPlanningFailed _ -> (
    "BuildUnitPlanningFailed",
    [ ("reason", Data.Json.String (Riot_build.error_message err)); ]
  )
  | Riot_build.CycleDetected { cycle_nodes } -> (
    "CycleDetected",
    [ ("cycle_nodes", Data.Json.Array (List.map cycle_nodes ~fn:Data.Json.string)); ]
  )
  | Riot_build.BuildAlreadyRunning { lock_path } -> (
    "BuildAlreadyRunning",
    [ ("lock_path", Data.Json.String (Path.to_string lock_path)); ]
  )
  | Riot_build.InvalidRequestedParallelism value -> (
    "InvalidRequestedParallelism",
    [ ("value", Data.Json.Int value); ]
  )
  | Riot_build.UnexpectedError { reason } -> (
    "UnexpectedError",
    [ ("reason", Data.Json.String reason); ]
  )

let send_build_error_state = fun ui err ->
  match ui.renderer with
  | Json_renderer ->
      let (kind, details) = build_error_json err in
      let message = Riot_build.error_message err in
      Json.write_event
        (Riot_model.Event.create
          ~session_id:(Riot_model.Session_id.make ())
          ~level:Riot_model.Event.Error
          (Riot_model.Event.Command (Riot_model.Event.CommandError { kind; details; message })))
  | Line_renderer -> Line.write_build_error err
  | Tui_renderer dashboard -> Tui.write_build_error dashboard err

let send_command_error_state = fun ui ~kind ~details ~message ->
  match ui.renderer with
  | Json_renderer ->
      Json.write_event
        (Riot_model.Event.create
          ~session_id:(Riot_model.Session_id.make ())
          ~level:Riot_model.Event.Error
          (Riot_model.Event.Command (Riot_model.Event.CommandError { kind; details; message })))
  | Line_renderer -> Line.write_command_error message
  | Tui_renderer dashboard -> Tui.write_command_error dashboard message

let send_build_finished_state = fun ui ~duration ~progress ->
  match ui.renderer with
  | Json_renderer -> ()
  | Line_renderer -> Line.write_build_finished ~duration ~progress
  | Tui_renderer dashboard -> Tui.write_build_finished dashboard ~duration ~progress

let handle_request = fun state request ->
  match request with
  | Send event -> send_state state event
  | Clear -> clear_state state
  | Build_error err -> send_build_error_state state err
  | Command_error { kind; details; message } ->
      send_command_error_state state ~kind ~details ~message
  | Build_finished { duration; progress } -> send_build_finished_state state ~duration ~progress

let rec loop = fun state ->
  let selector msg =
    match msg with
    | Riot_cli_ui_request envelope -> Select envelope
    | _ -> Skip
  in
  let envelope = receive ~selector () in
  let result =
    try
      handle_request state envelope.request;
      Ok ()
    with
    | exn -> Error exn
  in
  send envelope.reply_to (Riot_cli_ui_response { request_id = envelope.request_id; result });
  loop state

let make = fun ?profile ?workspace_root ~mode () ->
  {
    pid = spawn (fun () -> loop (make_state ?profile ?workspace_root ~mode ()));
  }

let await_response = fun request_id ->
  let selector msg =
    match msg with
    | Riot_cli_ui_response { request_id = got; result } when Int.equal got request_id ->
        Select result
    | _ -> Skip
  in
  match receive ~selector () with
  | Ok () -> ()
  | Error exn -> raise exn

let call = fun ui request ->
  let request_id = next_request_id () in
  send ui.pid (Riot_cli_ui_request { request; reply_to = self (); request_id });
  await_response request_id

let clear = fun ui -> call ui Clear

let send = fun ui event -> call ui (Send event)

let send_build_error = fun ui err -> call ui (Build_error err)

let send_command_error = fun ui ~kind ~details ~message ->
  call
    ui
    (Command_error { kind; details; message })

let failure_message = fun err ->
  match err with
  | Failure message -> message
  | _ -> Exception.to_string err

let send_failure = fun ?(kind = "CliError") ui err ->
  let message = failure_message err in
  send_command_error ui ~kind ~details:[ ("message", Data.Json.String message); ] ~message;
  Error err

let send_build_finished = fun ui ~duration ~progress ->
  call
    ui
    (Build_finished { duration; progress })

let reset_json_clock = Json.reset_clock

let display_package_name = Common.display_package_name

let planning_error_lines = Common.planning_error_lines

let build_unit_planning_error_lines = Common.build_unit_planning_error_lines

let build_failure_detail_lines = Common.build_failure_detail_lines

let workspace_fix_providers = fun (workspace: Riot_model.Workspace.t) ->
  workspace.packages
  |> List.filter ~fn:Riot_model.Package_manifest.is_workspace_member
  |> List.map ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> pkg.fix_providers)
  |> List.concat
