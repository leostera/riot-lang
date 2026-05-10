open Std
open Std.Collections

let format_deps_event = fun ~seen_registry_updates kind ->
  match kind with
  | Riot_model.Event.DepsRegistryIndexUpdating { registry } ->
      if HashSet.contains seen_registry_updates ~value:registry then
        None
      else
        (
          let _ = HashSet.insert seen_registry_updates ~value:registry in
          Some (Common.status_line Common.Terminal.Running ("updating " ^ registry ^ " index"))
        )
  | Riot_model.Event.DepsPackageResolvedForBuild _ -> None
  | Riot_model.Event.DepsPackageDownloadStarted { package; version; _ } ->
      Some (Common.status_line
        Common.Terminal.Running
        ("fetching " ^ Riot_model.Package_name.to_string package ^ " " ^ version))
  | Riot_model.Event.DepsPackageDownloadQueued { package; version; _ } ->
      Some (Common.status_line
        Common.Terminal.Running
        ("queued " ^ Riot_model.Package_name.to_string package ^ " (" ^ version ^ ")"))
  | Riot_model.Event.DepsResolutionStarted _
  | Riot_model.Event.DepsResolutionRefreshingLock _
  | Riot_model.Event.DepsResolutionFailed _
  | Riot_model.Event.DepsUniverseBuilding _
  | Riot_model.Event.DepsUniverseBuilt _
  | Riot_model.Event.DepsPackageMetadataFetchStarted _
  | Riot_model.Event.DepsPackageMetadataFetchFinished _
  | Riot_model.Event.DepsPackageMetadataFetchFailed _
  | Riot_model.Event.DepsSourceMaterializationFinished _
  | Riot_model.Event.DepsLockfileReadStarted _
  | Riot_model.Event.DepsLockfileReadFinished _
  | Riot_model.Event.DepsLockfileReadFailed _
  | Riot_model.Event.DepsLockfileWriteStarted _
  | Riot_model.Event.DepsLockfileWriteFinished _
  | Riot_model.Event.DepsLockfileWriteFailed _
  | Riot_model.Event.DepsResolutionFinished _
  | Riot_model.Event.DepsResolutionUsingExistingLock _
  | Riot_model.Event.DepsResolutionUnlocking _
  | Riot_model.Event.DepsPackageManifestFetchStarted _
  | Riot_model.Event.DepsPackageManifestFetchFinished _
  | Riot_model.Event.DepsPackageManifestFetchFailed _
  | Riot_model.Event.DepsPackageDownloadSkipped _
  | Riot_model.Event.DepsPackageMaterializationStarted _
  | Riot_model.Event.DepsPackageMaterializationFinished _
  | Riot_model.Event.DepsPackageMaterializationFailed _ -> None
  | Riot_model.Event.DepsSourceMaterializationStarted { source_locator; ref_ } ->
      Some (
        Common.status_line
          Common.Terminal.Running
          (
            "installing " ^ (
              match ref_ with
              | Some ref_ -> source_locator ^ "#" ^ ref_
              | None -> source_locator
            )
          )
      )
  | Riot_model.Event.DepsManifestUpdated {
      path;
      section;
      operation;
      dependency;
    } ->
      let verb =
        match operation with
        | `Add -> "added"
        | `Remove -> "removed"
      in
      Some (Common.status_line
        Common.Terminal.Success
        (verb ^ " " ^ dependency ^ " (" ^ section ^ ") in " ^ path))
  | Riot_model.Event.DepsPackageVersionLocked { package; version } ->
      Some (Common.status_line
        Common.Terminal.Success
        ("locked " ^ Riot_model.Package_name.to_string package ^ " (" ^ version ^ ")"))
  | Riot_model.Event.DepsPackageVersionsUnchanged _ ->
      Some (Common.status_line Common.Terminal.Success "dependencies are already up to date")
  | Riot_model.Event.DepsPackageVersionUpdated { package; from_version; to_version } ->
      Some (Common.status_line
        Common.Terminal.Success
        (Riot_model.Package_name.to_string package
        ^ " updated ("
        ^ from_version
        ^ " -> "
        ^ to_version
        ^ ")"))
  | kind -> Some (Riot_model.Event.display (Riot_model.Event.Deps kind))

let format_pm_event = fun ~seen_registry_updates kind ->
  match kind with
  | Riot_model.Event.Deps event -> format_deps_event ~seen_registry_updates event
  | kind -> Some (Riot_model.Event.display kind)

let write_pm_event = fun ~seen_registry_updates event ->
  match format_pm_event ~seen_registry_updates event.Riot_model.Event.kind with
  | Some message -> Common.out message
  | None -> ()

let write_command_error = fun human_message -> Common.out_status Common.Terminal.Error human_message

let command_binary_label = fun package binary ->
  Riot_model.Package_name.to_string package ^ ":" ^ binary

let write_command_event = fun ?workspace_root event ->
  match event with
  | Riot_model.Event.CommandBinaryRunning { package; binary; _ } ->
      Common.out_status Common.Terminal.Running (command_binary_label package binary)
  | Riot_model.Event.CommandBinaryInstalling { package; binary } ->
      Common.out_status
        Common.Terminal.Running
        ("installing " ^ command_binary_label package binary)
  | Riot_model.Event.CommandBinaryPromoted { binary; destination; _ } ->
      Common.out_status
        Common.Terminal.Success
        ("promoted " ^ binary ^ " to " ^ Common.display_path ?workspace_root destination)
  | Riot_model.Event.CommandBinaryInstalled { binary; duration_ms; mode; _ } ->
      let duration =
        Time.Duration.from_millis duration_ms
        |> Time.Duration.to_secs_string ~precision:2
      in
      Common.out_status Common.Terminal.Success ("installed " ^ binary ^ " in " ^ duration ^ "s");
      (
        match mode with
        | Riot_model.Event.CommandInstallGlobal -> Common.write_global_bin_path_hint ()
        | Riot_model.Event.CommandInstallLocal -> ()
      )
  | Riot_model.Event.CommandError error -> write_command_error error.message

let write_building_target_event = fun ~target ~host ->
  let target_name = Riot_model.Target.to_string target in
  if not host then
    Common.out_status Common.Terminal.Running ("cross-compiling for " ^ target_name)

let scaled_size_string = fun bytes divisor suffix ->
  let whole = Int64.div bytes divisor in
  let remainder = Int64.rem bytes divisor in
  let fraction = Int64.div (Int64.mul remainder 10L) divisor in
  Int64.to_string whole ^ "." ^ Int64.to_string fraction ^ " " ^ suffix

let size_to_string = fun size_bytes ->
  let kib = 1_024L in
  let mib = Int64.mul kib 1_024L in
  let gib = Int64.mul mib 1_024L in
  let tib = Int64.mul gib 1_024L in
  if Int64.compare size_bytes tib != Order.LT then
    scaled_size_string size_bytes tib "TiB"
  else if Int64.compare size_bytes gib != Order.LT then
    scaled_size_string size_bytes gib "GiB"
  else if Int64.compare size_bytes mib != Order.LT then
    scaled_size_string size_bytes mib "MiB"
  else if Int64.compare size_bytes kib != Order.LT then
    scaled_size_string size_bytes kib "KiB"
  else
    Int64.to_string size_bytes ^ " B"

let format_cache_gc_cleanup = fun (summary: Riot_model.Event.cache_gc_summary) ->
  Int.to_string summary.deleted_entries
  ^ " cache entries and "
  ^ Int.to_string summary.deleted_generations
  ^ " generations ("
  ^ size_to_string summary.size_before_bytes
  ^ " -> "
  ^ size_to_string summary.size_after_bytes
  ^ ")"

type cache_gc_progress = {
  total_entries: int;
  step: int;
  mutable removed_entries: int;
}

let cache_gc_progress = ref None

let max_cache_gc_progress_dots = 40

let start_cache_gc_progress = fun total_entries ->
  if total_entries > 0 then (
    let step =
      let raw = (total_entries + max_cache_gc_progress_dots - 1) / max_cache_gc_progress_dots in
      if raw > 0 then
        raw
      else
        1
    in
    eprint (Common.status_line Common.Terminal.Running "removing cache entries ");
    cache_gc_progress := Some { total_entries; step; removed_entries = 0 }
  )

let tick_cache_gc_progress = fun () ->
  match !cache_gc_progress with
  | None -> ()
  | Some progress ->
      progress.removed_entries <- progress.removed_entries + 1;
      if
        progress.removed_entries = progress.total_entries
        || progress.removed_entries mod progress.step = 0
      then
        eprint "."

let close_cache_gc_progress = fun () ->
  match !cache_gc_progress with
  | None -> ()
  | Some _ ->
      eprintln "";
      cache_gc_progress := None

let write_cache_gc_event = fun event ->
  match event with
  | Riot_model.Event.CacheBuildHit _
  | Riot_model.Event.CacheBuildMiss _
  | Riot_model.Event.CacheBuildStored _
  | Riot_model.Event.CacheStoreCreating
  | Riot_model.Event.CacheStoreCreated _ -> ()
  | Riot_model.Event.CacheGcStarted { trigger = Riot_model.Event.Manual } ->
      Common.out_status
        Common.Terminal.Running
        "running tracked cache GC (build root kept; use --force to remove it)"
  | Riot_model.Event.CacheGcStarted { trigger = Riot_model.Event.Post_build } -> ()
  | Riot_model.Event.CacheGcCacheScanStarted { trigger = Riot_model.Event.Manual; build_root } ->
      Common.out_status
        Common.Terminal.Running
        ("scanning tracked cache entries under " ^ Path.to_string build_root)
  | Riot_model.Event.CacheGcCacheScanStarted { trigger = Riot_model.Event.Post_build; _ } -> ()
  | Riot_model.Event.CacheGcCacheEntryScanStarted { trigger = Riot_model.Event.Manual; _ } -> ()
  | Riot_model.Event.CacheGcCacheEntryScanStarted { trigger = Riot_model.Event.Post_build; _ } -> ()
  | Riot_model.Event.CacheGcCacheEntryScanned { trigger = Riot_model.Event.Manual; _ } -> ()
  | Riot_model.Event.CacheGcCacheEntryScanned { trigger = Riot_model.Event.Post_build; _ } -> ()
  | Riot_model.Event.CacheGcCacheScanCompleted {
      trigger = Riot_model.Event.Manual;
      entry_count;
      total_size_bytes;
    } ->
      Common.out_status
        Common.Terminal.Success
        ("found "
        ^ Int.to_string entry_count
        ^ " tracked cache entries ("
        ^ size_to_string total_size_bytes
        ^ ")")
  | Riot_model.Event.CacheGcCacheScanCompleted { trigger = Riot_model.Event.Post_build; _ } -> ()
  | Riot_model.Event.CacheGcPlanComputed {
      trigger = Riot_model.Event.Manual;
      deleted_entries;
      deleted_generations;
      reclaimable_bytes;
    } ->
      Common.out_status
        Common.Terminal.Running
        ("removing "
        ^ Int.to_string deleted_entries
        ^ " cache entries and "
        ^ Int.to_string deleted_generations
        ^ " generations; reclaiming "
        ^ size_to_string reclaimable_bytes);
      start_cache_gc_progress deleted_entries
  | Riot_model.Event.CacheGcPlanComputed { trigger = Riot_model.Event.Post_build; _ } -> ()
  | Riot_model.Event.CacheGcCacheEntryDeleteStarted { trigger = Riot_model.Event.Manual; _ } ->
      tick_cache_gc_progress ()
  | Riot_model.Event.CacheGcCacheEntryDeleteStarted { trigger = Riot_model.Event.Post_build; _ } ->
      ()
  | Riot_model.Event.CacheGcGenerationDeleteStarted { trigger = Riot_model.Event.Manual; _ } -> ()
  | Riot_model.Event.CacheGcGenerationDeleteStarted { trigger = Riot_model.Event.Post_build; _ } ->
      ()
  | Riot_model.Event.CacheGcSkipped { trigger = Riot_model.Event.Post_build; _ } -> ()
  | Riot_model.Event.CacheGcSkipped { summary; _ } ->
      Common.out_status
        Common.Terminal.Skipped
        ("tracked cache is already within policy ("
        ^ size_to_string summary.size_after_bytes
        ^ "). Build root kept; use --force to remove it.")
  | Riot_model.Event.CacheGcCompleted { summary; _ } ->
      close_cache_gc_progress ();
      Common.out_status
        Common.Terminal.Success
        ("cleaned tracked cache: " ^ format_cache_gc_cleanup summary ^ ". Build root kept.")
  | Riot_model.Event.CacheGcFailed { error; _ } ->
      close_cache_gc_progress ();
      Common.out_status Common.Terminal.Error ("cache GC failed: " ^ error)
  | Riot_model.Event.CacheForceCleanStarted { build_root } ->
      Common.out_status Common.Terminal.Running ("removing build root " ^ Path.to_string build_root)
  | Riot_model.Event.CacheForceCleanCompleted { build_root } ->
      Common.out_status Common.Terminal.Success ("removed build root " ^ Path.to_string build_root)
  | Riot_model.Event.CacheForceCleanFailed { build_root; error } ->
      Common.out_status
        Common.Terminal.Error
        ("failed to remove build root " ^ Path.to_string build_root ^ ": " ^ error)

let write_build_kind = fun ?render_state ?profile event ->
  match event with
  | Riot_model.Event.BuildPackageCompilationStarted { package; build_target; _ } ->
      Common.out_status
        Common.Terminal.Building
        (Common.display_build_package_name ?render_state ?profile ~build_target package)
  | Riot_model.Event.BuildPackageFinished {
      package;
      build_target;
      status = Riot_model.Event.Fresh;
      _;
    } ->
      Common.out_status
        Common.Terminal.Built
        (Common.display_build_package_name ?render_state ?profile ~build_target package)
  | Riot_model.Event.BuildPackageFinished {
      package;
      build_target;
      status = Riot_model.Event.Cached;
      _;
    } ->
      Common.out_status
        Common.Terminal.Cached
        (Common.display_build_package_name ?render_state ?profile ~build_target package)
  | Riot_model.Event.BuildPackageSkippedDetailed { package; build_target; reason } ->
      Common.out_status
        Common.Terminal.Skipped
        (Common.display_build_package_name ?render_state ?profile ~build_target package
        ^ ": "
        ^ reason)
  | Riot_model.Event.BuildPackageFailed { package; build_target; error } ->
      Common.out_status
        Common.Terminal.Error
        (Common.display_build_package_name ?render_state ?profile ~build_target package
        ^ ": "
        ^ Common.build_package_error_message error)
  | Riot_model.Event.BuildPackageWarnings { package; build_target; messages; _ } ->
      messages
      |> List.for_each
        ~fn:(fun message ->
          Common.out_prefixed_payload
            ~prefix:(Common.status_line
              Common.Terminal.Warning
              (Common.display_build_package_name ?render_state ?profile ~build_target package ^ ": "))
            message)
  | Riot_model.Event.BuildTargetBuilding { target; host } ->
      write_building_target_event ~target ~host
  | _ -> ()

let write_phase_event = fun phase ->
  match phase with
  | Riot_model.Event.TargetsResolved _
  | Riot_model.Event.ToolchainsEnsured _
  | Riot_model.Event.ToolchainsValidated _
  | Riot_model.Event.RuntimeStarting
  | Riot_model.Event.RuntimeStarted -> ()
  | Riot_model.Event.BuildLockWaiting _ ->
      Common.out_status Common.Terminal.Running "build lock is taken, waiting..."
  | Riot_model.Event.PackagePlanningStarted _ -> ()
  | Riot_model.Event.PackagePlanStarted _ -> ()
  | Riot_model.Event.PackagePlanSourceStarted _ -> ()
  | Riot_model.Event.PackagePlanFinished _ -> ()
  | Riot_model.Event.PackagePlanningFinished _ -> ()
  | Riot_model.Event.PackageActionGraphPlanned _ -> ()
  | Riot_model.Event.BuildLanesPreparationStarted _
  | Riot_model.Event.BuildLanesPreparationFinished _
  | Riot_model.Event.BuildUnitPlanCreated _
  | Riot_model.Event.BuildLanePreparationStarted _
  | Riot_model.Event.BuildLaneLockAcquired _
  | Riot_model.Event.BuildLaneToolchainInitialized _
  | Riot_model.Event.BuildLaneStoreCreated _
  | Riot_model.Event.BuildLanePreparationFinished _ -> ()
  | Riot_model.Event.PackageExecutionStarted { package_count; _ } ->
      let _ = package_count in
      ()
  | Riot_model.Event.PackageExecutionFinished { built_count; failed_count; error_count; _ } ->
      if failed_count > 0 || error_count > 0 then
        Common.out_status
          Common.Terminal.Error
          ("execution failed: "
          ^ Common.build_count_summary
            ~built_count
            ~cached_count:0
            ~skipped_count:0
            ~failed_count
            ~error_count
            ())
  | Riot_model.Event.TargetBuildStarted _
  | Riot_model.Event.TargetBuildFinished _
  | Riot_model.Event.CacheGenerationRecordingStarted _
  | Riot_model.Event.CacheGenerationRecorded _
  | Riot_model.Event.ReturningResults _ -> ()

let write_event = fun ?render_state ?profile ?workspace_root ~seen_registry_updates event ->
  match event.Riot_model.Event.kind with
  | Riot_model.Event.Deps _ -> write_pm_event ~seen_registry_updates event
  | Riot_model.Event.Build (Riot_model.Event.BuildPhase phase) -> write_phase_event phase
  | Riot_model.Event.Build build_event -> write_build_kind ?render_state ?profile build_event
  | Riot_model.Event.Cache cache_event -> write_cache_gc_event cache_event
  | Riot_model.Event.Command command_event -> write_command_event ?workspace_root command_event
  | kind -> (
      match Riot_model.Event.display kind with
      | "" -> ()
      | message -> Common.out message
    )

let write_build_failed_error = fun errors ->
  match errors with
  | [] -> Common.out_status Common.Terminal.Error "build failed"
  | [ failure ] -> Common.write_failure_blocks [ failure ]
  | failures ->
      Common.out_status Common.Terminal.Error "build failed";
      Common.write_failure_blocks failures

let write_package_not_found_error = fun ~package_name ~available_packages ->
  let package_name = Riot_model.Package_name.to_string package_name in
  let available_packages = List.map available_packages ~fn:Riot_model.Package_name.to_string in
  Common.out_status Common.Terminal.Error ("package '" ^ package_name ^ "' not found");
  Common.out "";
  Common.out "Available packages:";
  List.for_each
    available_packages
    ~fn:(fun pkg -> Common.out (Jollyroger.Layout.bullet ~indent:2 pkg))

let write_packages_not_found_error = fun ~package_names ~available_packages ->
  let package_names = List.map package_names ~fn:Riot_model.Package_name.to_string in
  let available_packages = List.map available_packages ~fn:Riot_model.Package_name.to_string in
  Common.out_status
    Common.Terminal.Error
    ("packages not found: " ^ String.concat ", " package_names);
  Common.out "";
  Common.out "Available packages:";
  List.for_each
    available_packages
    ~fn:(fun pkg -> Common.out (Jollyroger.Layout.bullet ~indent:2 pkg))

let write_build_error = fun err ->
  match err with
  | Riot_build.TargetSelectionFailed _ -> write_command_error (Riot_build.error_message err)
  | Riot_build.PackageNotFound { package_name; available_packages } ->
      write_package_not_found_error ~package_name ~available_packages
  | Riot_build.PackagesNotFound { package_names; available_packages } ->
      write_packages_not_found_error ~package_names ~available_packages
  | Riot_build.ToolchainInstallFailed _
  | Riot_build.ToolchainInitializationFailed _ -> write_command_error (Riot_build.error_message err)
  | Riot_build.BuildFailed { errors } -> write_build_failed_error errors
  | Riot_build.BuildUnitPlanningFailed planning_error ->
      Common.build_unit_planning_error_lines planning_error
      |> List.for_each ~fn:Common.out
  | Riot_build.CycleDetected _
  | Riot_build.BuildAlreadyRunning _
  | Riot_build.InvalidRequestedParallelism _
  | Riot_build.UnexpectedError _ -> write_command_error (Riot_build.error_message err)

let write_build_finished = fun ~duration ~progress ->
  let formatted_duration = Time.Duration.to_secs_string ~precision:2 duration in
  let summary =
    Common.build_count_summary
      ~built_count:progress.Common.built_count
      ~cached_count:progress.cached_count
      ~skipped_count:progress.skipped_count
      ~failed_count:progress.failed_count
      ()
  in
  let status =
    if progress.failed_count = 0 && progress.skipped_count = 0 then
      Common.Terminal.Success
    else if progress.failed_count > 0 then
      Common.Terminal.Error
    else
      Common.Terminal.Warning
  in
  Common.out_status status ("finished in " ^ formatted_duration ^ "s (" ^ summary ^ ")")
