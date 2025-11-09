open Std
open Std.Collections
open Tusk_executor

(** Format a telemetry event for cargo-style output. Uses displayed_packages
    HashSet to track what we've already shown. *)
let format ~displayed_packages (event : Telemetry.event) =
  match event with
  (* Build lifecycle events *)
  | Telemetry_events.BuildStarted { package; _ } ->
      (* Show package starting to build *)
      if HashSet.contains displayed_packages package.name then ""
      else
        let _ = HashSet.insert displayed_packages package.name in
        "   \027[1;32mCompiling\027[0m " ^ package.name
  | Telemetry_events.BuildCompleted { package; status; duration; _ } -> (
      (* If we already showed it as BuildStarted, don't show again *)
      (* But if cached and not shown yet, show with (cached) *)
      match status with
      | `Cached ->
          if HashSet.contains displayed_packages package.name then ""
          else
            let _ = HashSet.insert displayed_packages package.name in
            "   \027[1;32mCompiling\027[0m " ^ package.name ^ " \027[1;90m(cached)\027[0m"
      | `Fresh -> "")
  | Telemetry_events.BuildFailed { package; error; _ } ->
      let error_msg =
        match error with
        | Telemetry_events.PlanningFailed planning_err -> (
            match planning_err with
            | Tusk_planner.Planning_error.CyclicDependency { cycle } ->
                "Cyclic dependency detected:\n         " ^
                  String.concat " ->\n         " cycle
            | _ ->
                "Planning failed: " ^
                  Tusk_planner.Planning_error.to_string planning_err)
        | Telemetry_events.ExecutionFailed { message } ->
            "Execution failed: " ^ message
        | Telemetry_events.ActionExecutionFailed { message } ->
            "Action failed: " ^ message
        | Telemetry_events.ActionOutputsNotCreated { missing } ->
            "Expected outputs not created: " ^
              String.concat ", " (List.map Path.to_string missing)
        | Telemetry_events.ActionDependenciesFailed { failed } ->
            "Dependencies failed: " ^ Int.to_string (List.length failed) ^ " actions"
      in
      "      \027[1;31mFailed\027[0m " ^ package.name ^ "\n" ^ error_msg
  | Telemetry_events.BuildSkipped { package; reason; _ } ->
      "     \027[1;33mSkipped\027[0m " ^ package.name ^ " (" ^ reason ^ ")"
  (* Cache events - these are action-level, not commonly emitted *)
  | Telemetry_events.CacheHit { package; _ } ->
      (* Only show if we haven't displayed this package yet *)
      if HashSet.contains displayed_packages package.name then ""
      else
        let _ = HashSet.insert displayed_packages package.name in
        "   \027[1;32mCompiling\027[0m " ^ package.name ^ " \027[1;90m(cached)\027[0m"
  | Telemetry_events.CacheMiss { package; _ } ->
      (* Only show if we haven't displayed this package yet *)
      if HashSet.contains displayed_packages package.name then ""
      else
        let _ = HashSet.insert displayed_packages package.name in
        "   \027[1;32mCompiling\027[0m " ^ package.name
  (* Action events - mostly silent, let package-level events show *)
  | Telemetry_events.ActionStarted _ -> ""
  | Telemetry_events.ActionCompleted _ -> ""
  | Telemetry_events.ActionFailed _ ->
      (* Don't show - BuildFailed will show the error *)
      ""
  (* Workspace events *)
  | Telemetry_events.WorkspaceStarted _ -> ""
  | Telemetry_events.WorkspaceCompleted
      { total_duration; cached_count; built_count; failed_count; _ } ->
      if failed_count = 0 then
        let total_secs =
          Float.of_int (Time.Duration.to_millis total_duration) /. 1000.0
        in
        let formatted_secs = 
          let rounded = Float.round (total_secs *. 100.0) /. 100.0 in
          Float.to_string rounded
        in
        "   \027[1;32mFinished\027[0m in " ^ formatted_secs ^ "s (" ^ 
          Int.to_string built_count ^ " built, " ^ Int.to_string cached_count ^ " cached)"
      else
        "   \027[1;31mFailed\027[0m with " ^ Int.to_string failed_count ^ " errors (" ^ 
          Int.to_string built_count ^ " built, " ^ Int.to_string cached_count ^ " cached)"
  (* Catch-all for unknown or future telemetry events *)
  | _ -> ""
