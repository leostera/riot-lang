open Std
open Std.Collections
open Tusk_model

type request =
  | Workspace
  | Package of string

type error =
  | ConflictingSelection
  | PublishFailed of Tusk_publish.publish_error

let out = eprintln

let command =
  let open ArgParser in
    let open Arg in command "publish"
    |> about "Publish packages to the registry"
    |> args
      [
        option "package" |> short 'p' |> long "package" |> help "Publish a specific workspace package";
        flag "workspace" |> long "workspace" |> help "Publish workspace packages in dependency order";
        flag "dry-run" |> long "dry-run" |> help "Run local publish checks without uploading";
        flag "skip-check" |> long "skip-check" |> help "Skip the `tusk fix --check` preflight step";
      ]

let message = function
  | ConflictingSelection -> "cannot combine --package with --workspace"
  | PublishFailed error -> Tusk_publish.publish_error_message error

let fail = fun err ->
  out ("\027[1;31mError\027[0m: " ^ message err);
  Error (Failure (message err))

let resolve_request = fun ~package_name ~workspace_mode ->
  match package_name, workspace_mode with
  | Some _, true -> Error ConflictingSelection
  | Some package, false -> Ok (Package package)
  | None, _ -> Ok Workspace

let publish_request = fun ~skip_check request ->
  let selection =
    match request with
    | Workspace -> Tusk_publish.Workspace
    | Package package -> Tusk_publish.Package package
  in
  Tusk_publish.{ selection; skip_check }

let relative_or_absolute = fun ~root path ->
  match Path.strip_prefix path ~prefix:root with
  | Ok rel -> Path.to_string rel
  | Error _ -> Path.to_string path

let write_block = fun text ->
  text
  |> String.split_on_char '\n'
  |> List.filter (fun line -> not (String.equal line ""))
  |> List.iter out

let format_fmt_file_result = fun ~root (result: Krasny.Runner.file_result) ->
  let status_char, suffix =
    match result.status, result.error with
    | Krasny.Runner.Failed, Some error -> ("\027[1;31m✗\027[0m", ": " ^ error)
    | Krasny.Runner.Failed, None -> ("\027[1;31m✗\027[0m", " (failed)")
    | Krasny.Runner.Already_formatted, _ -> ("\027[1;32m✓\027[0m", " (already formatted)")
    | Krasny.Runner.Needs_formatting, _ -> ("\027[1;33m!\027[0m", " (needs formatting)")
    | Krasny.Runner.Would_reformat, _ -> ("\027[1;32m✓\027[0m", " (would reformat safely)")
    | Krasny.Runner.Formatted, _ -> ("\027[1;32m✓\027[0m", " (formatted)")
    | Krasny.Runner.Unsafe_to_format, Some error -> (
      "\027[1;31m✗\027[0m",
      " (unsafe to format: " ^ error ^ ")"
    )
    | Krasny.Runner.Unsafe_to_format, None -> ("\027[1;31m✗\027[0m", " (unsafe to format)")
  in
  status_char ^ " " ^ relative_or_absolute ~root result.file ^ suffix

let format_fmt_summary = fun (summary: Krasny.Runner.summary) ->
  let status_char =
    if summary.needs_formatting = 0 && summary.failed_files = 0 then
      "\027[1;32m✓\027[0m"
    else
      "\027[1;31m✗\027[0m"
  in
  let duration = Time.Duration.to_secs_string ~precision:2 summary.duration in
  status_char
  ^ " Checked "
  ^ Int.to_string summary.total_files
  ^ " files in "
  ^ duration
  ^ "s ("
  ^ Int.to_string summary.already_formatted
  ^ " already formatted, "
  ^ Int.to_string summary.needs_formatting
  ^ " need formatting, "
  ^ Int.to_string summary.would_reformat
  ^ " would reformat safely, "
  ^ Int.to_string summary.unsafe_to_format
  ^ " unsafe to format, "
  ^ Int.to_string summary.formatted_files
  ^ " formatted, "
  ^ Int.to_string summary.failed_files
  ^ " failed)"

let format_fix_diagnostics = fun result ->
  let grouped = Tusk_fix.Diagnostic.group_diagnostics result.Tusk_fix.Runner.diagnostics in
  if List.length grouped > 0 then
    Some (Tusk_fix.Diagnostic.grouped_list_to_formatted_output
      ~file:result.file
      ~source:result.final_source
      grouped)
  else
    None

let format_fix_parse_diagnostics = fun result ->
  if List.length result.Tusk_fix.Runner.parse_diagnostics > 0 then
    Some ("  " ^ Int.to_string (List.length result.parse_diagnostics) ^ " parse diagnostics")
  else
    None

let write_fix_result = fun ~workspace_root (result: Tusk_fix.Runner.file_result) ->
  let rel = relative_or_absolute ~root:workspace_root result.file in
  match result.error with
  | Some error ->
      out ("\027[1;31m✗\027[0m " ^ rel);
      out ("  " ^ error)
  | None ->
      if result.changed then
        out
          ("\027[1;32m✓\027[0m "
          ^ rel
          ^ " (applied "
          ^ Int.to_string (List.length result.applied_fixes)
          ^ " safe fixes)");
      (
        match format_fix_parse_diagnostics result with
        | Some text ->
            out
              ("\027[1;31m✗\027[0m "
              ^ rel
              ^ " ("
              ^ Int.to_string (List.length result.parse_diagnostics)
              ^ " parse issues)");
            write_block text
        | None -> ()
      );
      (
        match format_fix_diagnostics result with
        | Some text ->
            out
              ("\027[1;31m✗\027[0m "
              ^ rel
              ^ " ("
              ^ Int.to_string (List.length result.diagnostics)
              ^ " issues found)");
            write_block text
        | None -> ()
      )

let write_fix_summary = fun (summary: Tusk_fix.Runner.summary) ->
  out "";
  if summary.remaining_diagnostics = 0 && summary.failed_files = 0 then
    out ("\027[1;32m✓\027[0m No issues found in " ^ Int.to_string summary.total_files ^ " files")
  else
    out
      ("\027[1;31m✗\027[0m Found "
      ^ Int.to_string summary.remaining_diagnostics
      ^ " issues across "
      ^ Int.to_string summary.total_files
      ^ " files")

let version_label = function
  | Some version -> Std.Version.to_string version
  | None -> "<missing version>"

let publish_page_url = fun ~package ~version -> "https://pkgs.ml/p/" ^ package ^ "/" ^ version

let render_formatting = fun ~package ~version -> "  \027[1;32mFormatting\027[0m " ^ package ^ " " ^ version

let render_checking = fun ~package ~version -> "    \027[1;32mChecking\027[0m " ^ package ^ " " ^ version

let render_compiling = fun ~package ~version -> "   \027[1;32mCompiling\027[0m " ^ package ^ " " ^ version

let render_packing = fun ~package ~version ~artifact_path ->
  "     \027[1;32mPacking\027[0m " ^ package ^ " " ^ version ^ " (" ^ artifact_path ^ ")"

let render_publishing = fun ~package ~version ->
  "  \027[1;32mPublishing\027[0m "
  ^ package
  ^ " "
  ^ version
  ^ " ("
  ^ publish_page_url ~package ~version
  ^ ")"

let render_skipping = fun ~package ~version ->
  "    \027[1;33mSkipping\027[0m " ^ package ^ " " ^ version ^ " (already published)"

let write_publish_event = fun ~workspace_root ~seen_registry_updates ~displayed_packages ~progress event ->
  match event with
  | Tusk_publish.Fmt (Krasny.Report.Start _) -> ()
  | Tusk_publish.Fmt (Krasny.Report.File _) -> ()
  | Tusk_publish.Fmt (Krasny.Report.Summary _) -> ()
  | Tusk_publish.Fix (Tusk_fix.Event.Start _) -> ()
  | Tusk_publish.Fix (Tusk_fix.Event.FileStarted _) -> ()
  | Tusk_publish.Fix (Tusk_fix.Event.FileProgress _) -> ()
  | Tusk_publish.Fix (Tusk_fix.Event.FileResult _) -> ()
  | Tusk_publish.Fix (Tusk_fix.Event.Summary _) -> ()
  | Tusk_publish.Build (Tusk_build.Pm event) -> Build.write_pm_event
    ~mode:Build.Human
    ~seen_registry_updates
    event
  | Tusk_publish.Build (Tusk_build.BuildingTarget { target; host }) -> Build.write_building_target_event
    ~mode:Build.Human
    ~target
    ~host
  | Tusk_publish.Build (Tusk_build.Streaming (Tusk_build.Client.BuildEvent (Tusk_executor.Telemetry_events.CompilationStarted _))) -> ()
  | Tusk_publish.Build (Tusk_build.Streaming event) -> Build.write_streaming_event
    ~mode:Build.Human
    ~displayed_packages
    ~progress
    event
  | Tusk_publish.CheckStarted { package; version; stage=`fmt } -> out
    (render_formatting ~package ~version:(version_label version))
  | Tusk_publish.CheckStarted { package; version; stage=`fix } -> out
    (render_checking ~package ~version:(version_label version))
  | Tusk_publish.CheckStarted { package; version; stage=`build } -> out
    (render_compiling ~package ~version:(version_label version))
  | Tusk_publish.CheckStarted _ -> ()
  | Tusk_publish.CheckFinished _ -> ()
  | Tusk_publish.Packing { package; version; artifact_path } -> out
    (render_packing
      ~package
      ~version:(Std.Version.to_string version)
      ~artifact_path:(relative_or_absolute ~root:workspace_root artifact_path))
  | Tusk_publish.SkippedAlreadyPublished { package; version } -> out
    (render_skipping ~package ~version:(Std.Version.to_string version))
  | Tusk_publish.DryRunPlanned prepared -> out
    (render_publishing
      ~package:prepared.package.name
      ~version:(Std.Version.to_string prepared.version))
  | Tusk_publish.PackagePublished published -> out
    (render_publishing ~package:published.package_name ~version:published.package_version)

let run = fun (workspace: Workspace.t) matches ->
  match resolve_request
    ~package_name:(ArgParser.get_one matches "package")
    ~workspace_mode:(ArgParser.get_flag matches "workspace") with
  | Error err -> fail err
  | Ok request ->
      let mode =
        if ArgParser.get_flag matches "dry-run" then
          Tusk_publish.DryRun
        else
          Tusk_publish.Publish
      in
      let seen_registry_updates = HashSet.create () in
      let displayed_packages = HashSet.create () in
      let progress = Build.{ built_count = 0; cached_count = 0; failed_count = 0; skipped_count = 0 } in
      match Tusk_publish.publish
        ~on_event:(write_publish_event
          ~workspace_root:workspace.root
          ~seen_registry_updates
          ~displayed_packages
          ~progress)
        ~workspace
        ~request:(publish_request ~skip_check:(ArgParser.get_flag matches "skip-check") request)
        ~mode
        () with
      | Error err -> fail (PublishFailed err)
      | Ok _results -> Ok ()
