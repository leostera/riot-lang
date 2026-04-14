open Std
open Std.Collections
open Riot_executor

let format_prefixed_block = fun ~prefix message ->
  let trimmed = String.trim message in
  match String.split trimmed ~by:"\n" with
  | [] -> prefix
  | first :: rest ->
      prefix ^ first ^ (
        match rest with
        | [] -> ""
        | _ -> "\n" ^ String.concat "\n" rest
      )

(** Format a telemetry event for cargo-style output. Uses displayed_packages
    HashSet to track what we've already shown. *)
let format = fun ~displayed_packages (event: Telemetry.event) ->
  match event with
  | Telemetry_events.BuildStarted { package; _ } ->
      (* BuildStarted fires for all packages, but don't show anything yet *)
      ""
  | Telemetry_events.CompilationStarted { package; _ } ->
      (* Only show "Compiling" when actual compilation begins (fresh builds) *)
      if HashSet.contains displayed_packages ~value:package.name then
        ""
      else
        let _ = HashSet.insert displayed_packages ~value:package.name in
        "   \027[1;32mCompiling\027[0m " ^ Riot_model.Package_name.to_string package.name
  | Telemetry_events.BuildCompleted { package; status; duration; _ } -> (
      (* Cached packages should stay silent here. Only fresh compilation gets a
         "Compiling" line through CompilationStarted. *)
      match status with
      | `Cached -> ""
      | `Fresh -> ""
    )
  | Telemetry_events.PackageOcamlcWarnings { package; messages; _ } ->
      String.concat
        "\n"
        (List.map messages ~fn:(fun message ->
           format_prefixed_block
             ~prefix:("      \027[1;33mWarning\027[0m "
             ^ Riot_model.Package_name.to_string package.name
             ^ ": ")
             message))
  | Telemetry_events.BuildFailed { package; error; _ } ->
      let error_msg =
        match error with
        | Telemetry_events.PlanningFailed planning_err -> (
            match planning_err with
            | Riot_planner.Planning_error.CyclicDependency { cycle } -> "Cyclic dependency detected:\n         "
            ^ String.concat " ->\n         " cycle
            | _ -> "Planning failed: " ^ Riot_planner.Planning_error.to_string planning_err
          )
        | Telemetry_events.ExecutionFailed { message } ->
            "Execution failed: " ^ message
        | Telemetry_events.ActionExecutionFailed { message } ->
            "Action failed: " ^ message
        | Telemetry_events.ActionOutputsNotCreated { missing } ->
            "Expected outputs not created: "
            ^ String.concat ", " (List.map missing ~fn:Path.to_string)
        | Telemetry_events.ActionDependenciesFailed { failed } ->
            "Dependencies failed: " ^ Int.to_string (List.length failed) ^ " actions"
      in
      "      \027[1;31mFailed\027[0m "
      ^ Riot_model.Package_name.to_string package.name
      ^ "\n"
      ^ format_prefixed_block
          ~prefix:("      \027[1;31mError\027[0m "
          ^ Riot_model.Package_name.to_string package.name
          ^ ": ")
          error_msg
  | Telemetry_events.BuildSkipped { package; reason; _ } ->
      "     \027[1;33mSkipped\027[0m "
      ^ Riot_model.Package_name.to_string package.name
      ^ " ("
      ^ reason
      ^ ")"
  | Telemetry_events.CacheHit { package; _ } ->
      ""
  | Telemetry_events.CacheMiss { package; _ } ->
      ""
  | Telemetry_events.ActionStarted _ ->
      ""
  | Telemetry_events.ActionCommandStarted _ ->
      ""
  | Telemetry_events.ActionCompleted _ ->
      ""
  | Telemetry_events.ActionFailed _ ->
      (* Don't show - BuildFailed will show the error *)
      ""
  | Telemetry_events.WorkspaceStarted _ ->
      ""
  | Telemetry_events.WorkspaceCompleted _ ->
      ""
  | _ ->
      ""
