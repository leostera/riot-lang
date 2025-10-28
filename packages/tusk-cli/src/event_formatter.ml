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
        format "   \027[1;32mCompiling\027[0m %s" package.name
  | Telemetry_events.BuildCompleted { package; status; duration; _ } -> (
      (* If we already showed it as BuildStarted, don't show again *)
      (* But if cached and not shown yet, show with (cached) *)
      match status with
      | `Cached ->
          if HashSet.contains displayed_packages package.name then ""
          else
            let _ = HashSet.insert displayed_packages package.name in
            format "   \027[1;32mCompiling\027[0m %s \027[1;90m(cached)\027[0m"
              package.name
      | `Fresh -> "")
  | Telemetry_events.BuildFailed { package; error; _ } ->
      format "      \027[1;31mFailed\027[0m %s\n%s" package.name error
  | Telemetry_events.BuildSkipped { package; reason; _ } ->
      format "     \027[1;33mSkipped\027[0m %s (%s)" package.name reason
  (* Cache events - these are action-level, not commonly emitted *)
  | Telemetry_events.CacheHit { package; _ } ->
      (* Only show if we haven't displayed this package yet *)
      if HashSet.contains displayed_packages package.name then ""
      else
        let _ = HashSet.insert displayed_packages package.name in
        format "   \027[1;32mCompiling\027[0m %s \027[1;90m(cached)\027[0m"
          package.name
  | Telemetry_events.CacheMiss { package; _ } ->
      (* Only show if we haven't displayed this package yet *)
      if HashSet.contains displayed_packages package.name then ""
      else
        let _ = HashSet.insert displayed_packages package.name in
        format "   \027[1;32mCompiling\027[0m %s" package.name
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
          float_of_int (Time.Duration.to_millis total_duration) /. 1000.0
        in
        format "   \027[1;32mFinished\027[0m in %.2fs (%d built, %d cached)"
          total_secs built_count cached_count
      else
        format "   \027[1;31mFailed\027[0m with %d errors (%d built, %d cached)"
          failed_count built_count cached_count
  (* Catch-all for unknown or future telemetry events *)
  | _ -> ""
