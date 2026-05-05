open Std
open Std.Result.Syntax

module Build_core = Riot_build.Internal.Build_core
module Build_unit = Riot_planner.Build_unit
module Build_unit_graph = Riot_planner.Build_unit_graph
module Build_unit_plan = Riot_build.Internal.Build_unit_plan

let command =
  let open ArgParser in
  command "plan"
  |> about "Print the build-unit plan without checking cache or executing packages"
  |> args Build.build_args

let output_mode_of_request = fun (request: Build.request) -> request.output_mode

let out = println

let err = eprintln

let duration_us = fun ~started_at ~finished_at ->
  Time.Instant.saturating_duration_since ~earlier:started_at finished_at
  |> Time.Duration.to_micros

let target_strings = fun targets ->
  Riot_model.Target.Set.to_list targets
  |> List.sort ~compare:Riot_model.Target.compare
  |> List.map ~fn:Riot_model.Target.to_string

let scope_to_string = fun __tmp1 ->
  match __tmp1 with
  | Riot_build.Request.Runtime -> "runtime"
  | Dev -> "dev"

let unit_key_json = fun (key: Build_unit.key) ->
  Data.Json.Object [
    ("package", Data.Json.String (Riot_model.Package_name.to_string key.package));
    ("artifact", Data.Json.String (Build_unit.artifact_kind_to_string key.artifact));
    ("target", Data.Json.String (Riot_model.Target.to_string key.target));
    ("profile", Data.Json.String key.profile.name);
  ]

let unit_json = fun graph unit ->
  let key = Build_unit.key unit in
  Data.Json.Object [
    ("id", Data.Json.String (Crypto.Digest.hex (Build_unit.id unit)));
    ("key", unit_key_json key);
    (
      "dependencies",
      Data.Json.Array (
        Build_unit_graph.dependencies graph key
        |> List.map ~fn:unit_key_json
      )
    );
  ]

let plan_json = fun ~resolve_us ~create_us resolved plan ->
  let graph = Build_unit_plan.graph plan in
  let units = Build_unit_plan.units plan in
  Data.Json.Object [
    ("type", Data.Json.String "plan");
    ("unit_count", Data.Json.Int (List.length units));
    (
      "targets",
      Data.Json.Array (
        target_strings (Riot_build.Internal.Resolved_build.targets resolved)
        |> List.map ~fn:(fun target -> Data.Json.String target)
      )
    );
    (
      "packages",
      Data.Json.Array (
        Riot_build.Internal.Resolved_build.package_names resolved
        |> List.map
          ~fn:(fun package -> Data.Json.String (Riot_model.Package_name.to_string package))
      )
    );
    ("scope", Data.Json.String (scope_to_string (Riot_build.Internal.Resolved_build.scope resolved)));
    ("resolve_us", Data.Json.Int resolve_us);
    ("create_us", Data.Json.Int create_us);
    ("units", Data.Json.Array (List.map units ~fn:(unit_json graph)));
  ]

let print_human_plan = fun ~resolve_us ~create_us resolved plan ->
  let graph = Build_unit_plan.graph plan in
  let units = Build_unit_plan.units plan in
  out
    ("plan "
    ^ Int.to_string (List.length units)
    ^ " build unit(s)"
    ^ " resolve="
    ^ Int.to_string resolve_us
    ^ "us"
    ^ " create="
    ^ Int.to_string create_us
    ^ "us");
  out
    ("targets: "
    ^ String.concat ", " (target_strings (Riot_build.Internal.Resolved_build.targets resolved)));
  out ("scope: " ^ scope_to_string (Riot_build.Internal.Resolved_build.scope resolved));
  List.for_each
    units
    ~fn:(fun unit ->
      let key = Build_unit.key unit in
      out ("- " ^ Build_unit.key_to_string key);
      (
        match Build_unit_graph.dependencies graph key with
        | [] -> ()
        | dependencies ->
            out
              ("  deps: "
              ^ String.concat ", " (List.map dependencies ~fn:Build_unit.key_to_string))
      ))

let write_error = fun ~mode message ->
  match mode with
  | Build.Json ->
      out
        (Data.Json.to_string
          (Data.Json.Object [
            ("type", Data.Json.String "error");
            ("message", Data.Json.String message);
          ]))
  | Human -> err ("\027[1;31merror\027[0m " ^ message)

let planning_error_message = fun __tmp1 ->
  match __tmp1 with
  | Build_core.BuildUnitPlanningFailed planning_error ->
      Build.build_unit_planning_error_lines planning_error
      |> String.concat "\n"
  | err -> Build_core.error_message err

let run_request = fun (request: Build.request) ->
  let mode = output_mode_of_request request in
  let build_request =
    Riot_build.Request.make
      ~workspace:request.workspace
      ~packages:request.packages
      ~targets:request.targets
      ~scope:request.scope
      ~dev_artifacts:request.dev_artifacts
      ~profile:request.profile
      ~requested_parallelism:request.requested_parallelism
      ()
  in
  let result =
    let* context = Build_core.make_context build_request in
    let resolve_started_at = Time.Instant.now () in
    let* resolved = Build_core.resolve context build_request in
    let resolve_finished_at = Time.Instant.now () in
    let create_started_at = Time.Instant.now () in
    let* plan =
      Build_unit_plan.create context resolved
      |> Result.map_err ~fn:(fun err -> Build_core.BuildUnitPlanningFailed err)
    in
    let create_finished_at = Time.Instant.now () in
    Ok (
      resolved,
      plan,
      duration_us ~started_at:resolve_started_at ~finished_at:resolve_finished_at,
      duration_us ~started_at:create_started_at ~finished_at:create_finished_at
    )
  in
  match result with
  | Error err ->
      write_error ~mode (planning_error_message err);
      Error (Failure (Build_core.error_message err))
  | Ok (resolved, plan, resolve_us, create_us) ->
      (
        match mode with
        | Build.Json -> out (Data.Json.to_string (plan_json ~resolve_us ~create_us resolved plan))
        | Human -> print_human_plan ~resolve_us ~create_us resolved plan
      );
      Ok ()

let run = fun ~workspace matches ->
  match Build.request_of_matches ~workspace matches with
  | Error _ as err -> err
  | Ok request -> run_request request
