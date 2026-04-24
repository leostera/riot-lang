open Std
open Std.Result.Syntax
open Std.Collections
open Riot_model

type request =
  | Workspace
  | Package of Package_name.t

type error =
  | ConflictingSelection
  | PublishFailed of Riot_publish.publish_error

let out = eprintln

let command =
  let open ArgParser in
    let open ArgParser.Arg in command "publish"
    |> about "Publish packages to the registry"
    |> args
      [
        option "package" |> short 'p' |> long "package" |> help "Publish a specific workspace package";
        flag "workspace" |> long "workspace" |> help "Publish workspace packages in dependency order";
        flag "dry-run" |> long "dry-run" |> help "Run local publish checks without uploading";
        flag "skip-check" |> long "skip-check" |> help "Skip the `riot fix --check` preflight step";
      ]

let message = function
  | ConflictingSelection -> "cannot combine --package with --workspace"
  | PublishFailed error -> Riot_publish.publish_error_message error

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
    | Workspace -> Riot_publish.Workspace
    | Package package -> Riot_publish.Package package
  in
  Riot_publish.{ selection; skip_check }

let relative_or_absolute = fun ~root path ->
  match Path.strip_prefix path ~prefix:root with
  | Ok rel -> Path.to_string rel
  | Error _ -> Path.to_string path

let write_block = fun text ->
  text
  |> String.split ~by:"\n"
  |> List.filter ~fn:(fun line -> not (String.equal line ""))
  |> List.for_each ~fn:out

let format_fix_diagnostics = fun result ->
  let grouped = Riot_fix.Diagnostic.group_diagnostics result.Riot_fix.Runner.diagnostics in
  if List.length grouped > 0 then
    Some (Riot_fix.Diagnostic.grouped_list_to_formatted_output
      ~file:result.file
      ~source:result.final_source
      grouped)
  else
    None

let format_fix_parse_diagnostics = fun result ->
  if List.length result.Riot_fix.Runner.parse_diagnostics > 0 then
    Some ("  " ^ Int.to_string (List.length result.parse_diagnostics) ^ " parse diagnostics")
  else
    None

let write_fix_result = fun ~workspace_root (result: Riot_fix.Runner.file_result) ->
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

let write_fix_summary = fun (summary: Riot_fix.Runner.summary) ->
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

let render_skipping_not_public = fun ~package ~version ->
  "    \027[1;33mSkipping\027[0m " ^ package ^ " " ^ version ^ " (package is not public)"

let write_publish_event = fun ~workspace_root ~seen_registry_updates ~displayed_packages ~progress event ->
  match event with
  | Riot_publish.Fmt _ -> ()
  | Riot_publish.Fix (Riot_fix.Event.Start _) -> ()
  | Riot_publish.Fix (Riot_fix.Event.FileStarted _) -> ()
  | Riot_publish.Fix (Riot_fix.Event.FileProgress _) -> ()
  | Riot_publish.Fix (Riot_fix.Event.FileResult _) -> ()
  | Riot_publish.Fix (Riot_fix.Event.Summary _) -> ()
  | Riot_publish.Build build_event -> Build.write_build_event
    ~mode:Build.Human
    ~seen_registry_updates
    build_event
  | Riot_publish.CheckStarted { package; version; stage=`fmt } -> out
    (render_formatting ~package:(Package_name.to_string package) ~version:(version_label version))
  | Riot_publish.CheckStarted { package; version; stage=`fix } -> out
    (render_checking ~package:(Package_name.to_string package) ~version:(version_label version))
  | Riot_publish.CheckStarted { package; version; stage=`build } -> out
    (render_compiling ~package:(Package_name.to_string package) ~version:(version_label version))
  | Riot_publish.CheckStarted _ -> ()
  | Riot_publish.CheckFinished _ -> ()
  | Riot_publish.Packing { package; version; artifact_path } -> out
    (render_packing
      ~package:(Package_name.to_string package)
      ~version:(Std.Version.to_string version)
      ~artifact_path:(relative_or_absolute ~root:workspace_root artifact_path))
  | Riot_publish.SkippedNotPublic { package; version } -> out
    (render_skipping_not_public
      ~package:(Package_name.to_string package)
      ~version:(version_label version))
  | Riot_publish.SkippedAlreadyPublished { package; version } -> out
    (render_skipping
      ~package:(Package_name.to_string package)
      ~version:(Std.Version.to_string version))
  | Riot_publish.DryRunPlanned prepared -> out
    (render_publishing
      ~package:(Riot_model.Package_name.to_string prepared.package.name)
      ~version:(Std.Version.to_string prepared.version))
  | Riot_publish.PackagePublished published -> out
    (render_publishing ~package:published.package_name ~version:published.package_version)

let run = fun (workspace: Workspace.t) matches ->
  let package_name =
    match ArgParser.get_one matches "package" with
    | None -> Ok None
    | Some package_name -> Package_name.from_string package_name
    |> Result.map ~fn:Option.some
    |> Result.map_err ~fn:(fun error -> Failure (Package_name.error_message error))
  in
  let* package_name = package_name in
  match resolve_request ~package_name ~workspace_mode:(ArgParser.get_flag matches "workspace") with
  | Error err -> fail err
  | Ok request ->
      let mode =
        if ArgParser.get_flag matches "dry-run" then
          Riot_publish.DryRun
        else
          Riot_publish.Publish
      in
      let seen_registry_updates = HashSet.create () in
      let displayed_packages = HashSet.create () in
      let progress = Build.{ built_count = 0; cached_count = 0; failed_count = 0; skipped_count = 0 } in
      match Riot_publish.publish
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
