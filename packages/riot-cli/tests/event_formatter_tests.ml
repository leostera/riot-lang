open Std
open Std.Collections

module Test = Std.Test

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let test_phase_events_use_display_text = fun _ctx ->
  let displayed_packages = HashSet.create () in
  let event =
    Riot_build.Event.phase
      ~session_id:(Riot_model.Session_id.make ())
      Riot_build.Event.RuntimeStarted
  in
  let rendered = Riot_cli.Event_formatter.format ~displayed_packages event in
  if String.equal rendered "Build phase: runtime_started" then
    Ok ()
  else
    Error ("expected phase display text, got: " ^ rendered)

let test_building_target_mentions_target = fun _ctx ->
  let displayed_packages = HashSet.create () in
  let target =
    Riot_model.Target.from_string "aarch64-apple-darwin"
    |> Result.expect ~msg:"invalid target"
  in
  let event =
    Riot_model.Event.create
      ~session_id:(Riot_model.Session_id.make ())
      ~level:Riot_model.Event.Info
      (Riot_model.Event.Build (Riot_model.Event.BuildTargetBuilding { target; host = false }))
  in
  let rendered = Riot_cli.Event_formatter.format ~displayed_packages event in
  if String.contains rendered "aarch64-apple-darwin" then
    Ok ()
  else
    Error ("expected rendered target name, got: " ^ rendered)

let test_pm_events_use_display_text = fun _ctx ->
  let displayed_packages = HashSet.create () in
  let event =
    Riot_model.Event.create
      ~session_id:(Riot_model.Session_id.make ())
      ~level:Riot_model.Event.Info
      (Riot_model.Event.Deps (Riot_model.Event.DepsPackageVersionLocked {
        package = package_name "std";
        version = "1.0.0";
      }))
  in
  let rendered = Riot_cli.Event_formatter.format ~displayed_packages event in
  if String.contains rendered "std" then
    Ok ()
  else
    Error ("expected package name in PM rendering, got: " ^ rendered)

let tests =
  Test.[
    case "event formatter: phase events use display text" test_phase_events_use_display_text;
    case "event formatter: building target mentions target" test_building_target_mentions_target;
    case "event formatter: pm events use display text" test_pm_events_use_display_text;
  ]

let name = "Riot CLI Event Formatter Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
