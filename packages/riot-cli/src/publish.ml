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
  let open ArgParser.Arg in
  command "publish"
  |> about "Publish packages to the registry"
  |> args
    [
      option "package"
      |> short 'p'
      |> long "package"
      |> help "Publish a specific workspace package";
      flag "workspace"
      |> long "workspace"
      |> help "Publish workspace packages in dependency order";
      flag "dry-run"
      |> long "dry-run"
      |> help "Run local publish checks without uploading";
      flag "skip-check"
      |> long "skip-check"
      |> help "Skip the `riot fix --check` preflight step";
      flag "skip-fmt"
      |> long "skip-fmt"
      |> help "Skip the `riot fmt --check` preflight step";
      flag "json"
      |> long "json"
      |> help "Emit machine-readable JSONL events";
    ]

let message = fun __tmp1 ->
  match __tmp1 with
  | ConflictingSelection -> "cannot combine --package with --workspace"
  | PublishFailed error -> Riot_publish.publish_error_message error

let fail = fun err ->
  out ("\027[1;31mError\027[0m: " ^ message err);
  Error (Failure (message err))

let resolve_request = fun ~package_name ~workspace_mode ->
  match (package_name, workspace_mode) with
  | (Some _, true) -> Error ConflictingSelection
  | (Some package, false) -> Ok (Package package)
  | (None, _) -> Ok Workspace

let publish_request = fun ~skip_fmt ~skip_check request ->
  let selection =
    match request with
    | Workspace -> Riot_publish.Workspace
    | Package package -> Riot_publish.Package package
  in
  Riot_publish.{ selection; skip_fmt; skip_check }

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

let version_label = fun __tmp1 ->
  match __tmp1 with
  | Some version -> Std.Version.to_string version
  | None -> "<missing version>"

let publish_page_url = fun ~package ~version -> "https://pkgs.ml/p/" ^ package ^ "/" ^ version

let render_formatting = fun ~package ~version ->
  "  \027[1;32mFormatting\027[0m " ^ package ^ " " ^ version

let render_checking = fun ~package ~version ->
  "    \027[1;32mChecking\027[0m " ^ package ^ " " ^ version

let render_resolving = fun ~package ~version ->
  "   \027[1;32mResolving\027[0m " ^ package ^ " " ^ version

let render_compiling = fun ~package ~version ->
  "   \027[1;32mCompiling\027[0m " ^ package ^ " " ^ version

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

let write_publish_event = fun ~workspace_root ~ui ~displayed_packages ~progress event ->
  match event with
  | Riot_publish.Fmt _ -> ()
  | Riot_publish.Fix (Riot_fix.Event.Start _) -> ()
  | Riot_publish.Fix (Riot_fix.Event.FileStarted _) -> ()
  | Riot_publish.Fix (Riot_fix.Event.FileProgress _) -> ()
  | Riot_publish.Fix (Riot_fix.Event.FileResult _) -> ()
  | Riot_publish.Fix (Riot_fix.Event.Summary _) -> ()
  | Riot_publish.Build build_event -> Ui.send ui build_event
  | Riot_publish.CheckStarted { package; version; stage = `availability } ->
      out
        (render_resolving ~package:(Package_name.to_string package) ~version:(version_label version))
  | Riot_publish.CheckStarted { package; version; stage = `fmt } ->
      out
        (render_formatting
          ~package:(Package_name.to_string package)
          ~version:(version_label version))
  | Riot_publish.CheckStarted { package; version; stage = `fix } ->
      out
        (render_checking ~package:(Package_name.to_string package) ~version:(version_label version))
  | Riot_publish.CheckStarted { package; version; stage = `build } ->
      out
        (render_compiling ~package:(Package_name.to_string package) ~version:(version_label version))
  | Riot_publish.CheckStarted _ -> ()
  | Riot_publish.CheckFinished _ -> ()
  | Riot_publish.Packing { package; version; artifact_path } ->
      out
        (render_packing
          ~package:(Package_name.to_string package)
          ~version:(Std.Version.to_string version)
          ~artifact_path:(relative_or_absolute ~root:workspace_root artifact_path))
  | Riot_publish.SkippedNotPublic { package; version } ->
      out
        (render_skipping_not_public
          ~package:(Package_name.to_string package)
          ~version:(version_label version))
  | Riot_publish.SkippedAlreadyPublished { package; version } ->
      out
        (render_skipping
          ~package:(Package_name.to_string package)
          ~version:(Std.Version.to_string version))
  | Riot_publish.DryRunPlanned prepared ->
      out
        (render_publishing
          ~package:(Riot_model.Package_name.to_string prepared.package.name)
          ~version:(Std.Version.to_string prepared.version))
  | Riot_publish.PackagePublished published ->
      out (render_publishing ~package:published.package_name ~version:published.package_version)

let json_version_or_null = fun __tmp1 ->
  match __tmp1 with
  | Some version -> Data.Json.String (Std.Version.to_string version)
  | None -> Data.Json.Null

let publish_stage_json = fun __tmp1 ->
  match __tmp1 with
  | `availability -> Data.Json.String "availability"
  | `fmt -> Data.Json.String "fmt"
  | `fix -> Data.Json.String "fix"
  | `build -> Data.Json.String "build"
  | `metadata -> Data.Json.String "metadata"

let published_location_json = fun (location: Pkgs_ml.Registry.published_artifact_location) ->
  Data.Json.Object [
    ("key", Data.Json.String location.key);
    ("url", Data.Json.String location.url);
  ]

let published_record_json = fun (record: Pkgs_ml.Registry.published_record) ->
  Data.Json.Object [
    ("key", Data.Json.String record.key);
    ("created", Data.Json.Bool record.created);
  ]

let published_release_json = fun (release: Pkgs_ml.Registry.published_release) ->
  Data.Json.Object [
    ("package", Data.Json.String release.package_name);
    ("version", Data.Json.String release.package_version);
    ("artifact_sha256", Data.Json.String release.artifact_sha256);
    ("manifest", published_location_json release.manifest);
    ("source_archive", published_location_json release.source_archive);
    ("claim", published_record_json release.claim);
    ("release", published_record_json release.release);
    (
      "materialization",
      Data.Json.Object [
        ("manifest", Data.Json.Bool release.materialization.manifest);
        ("source", Data.Json.Bool release.materialization.source);
      ]
    );
  ]

let prepared_publish_json = fun ~workspace_root (prepared: Riot_deps.Publisher.prepared_publish) ->
  Data.Json.Object [
    ("package", Data.Json.String (Package_name.to_string prepared.package.name));
    ("version", Data.Json.String (Std.Version.to_string prepared.version));
    ("locator", Data.Json.String prepared.locator);
    ("selector", Data.Json.String prepared.selector);
    (
      "artifact_path",
      Data.Json.String (relative_or_absolute ~root:workspace_root prepared.artifact_path)
    );
  ]

let json_event = fun kind fields -> Data.Json.Object (("type", Data.Json.String kind) :: fields)

let publish_event_to_json = fun ~workspace_root event ->
  match event with
  | Riot_publish.Fmt event ->
      Some (json_event
        "publish.fmt"
        [ ("event", Riot_fmt.event_to_json ~root:workspace_root event); ])
  | Riot_publish.Fix event ->
      Some (json_event "publish.fix" [ ("event", Riot_fix.Event.to_json event); ])
  | Riot_publish.Build build_event -> Riot_build.Event.to_json build_event
  | Riot_publish.CheckStarted { package; version; stage } ->
      Some (json_event
        "publish.check.started"
        [
          ("package", Data.Json.String (Package_name.to_string package));
          ("version", json_version_or_null version);
          ("stage", publish_stage_json stage);
        ])
  | Riot_publish.CheckFinished { package; version; stage } ->
      Some (json_event
        "publish.check.finished"
        [
          ("package", Data.Json.String (Package_name.to_string package));
          ("version", json_version_or_null version);
          ("stage", publish_stage_json stage);
        ])
  | Riot_publish.Packing { package; version; artifact_path } ->
      Some (json_event
        "publish.packing"
        [
          ("package", Data.Json.String (Package_name.to_string package));
          ("version", Data.Json.String (Std.Version.to_string version));
          (
            "artifact_path",
            Data.Json.String (relative_or_absolute ~root:workspace_root artifact_path)
          );
        ])
  | Riot_publish.SkippedNotPublic { package; version } ->
      Some (json_event
        "publish.skipped"
        [
          ("package", Data.Json.String (Package_name.to_string package));
          ("version", json_version_or_null version);
          ("reason", Data.Json.String "not_public");
        ])
  | Riot_publish.SkippedAlreadyPublished { package; version } ->
      Some (json_event
        "publish.skipped"
        [
          ("package", Data.Json.String (Package_name.to_string package));
          ("version", Data.Json.String (Std.Version.to_string version));
          ("reason", Data.Json.String "already_published");
        ])
  | Riot_publish.DryRunPlanned prepared ->
      Some (json_event
        "publish.planned"
        [ ("publish", prepared_publish_json ~workspace_root prepared); ])
  | Riot_publish.PackagePublished published ->
      Some (json_event "publish.published" [ ("release", published_release_json published); ])

let publish_outcome_json = fun ~workspace_root outcome ->
  match outcome with
  | Riot_publish.SkippedNotPublicPackage { package; version } ->
      Data.Json.Object [
        ("status", Data.Json.String "skipped");
        ("package", Data.Json.String (Package_name.to_string package));
        ("version", json_version_or_null version);
        ("reason", Data.Json.String "not_public");
      ]
  | Riot_publish.Skipped { package; version } ->
      Data.Json.Object [
        ("status", Data.Json.String "skipped");
        ("package", Data.Json.String (Package_name.to_string package));
        ("version", Data.Json.String (Std.Version.to_string version));
        ("reason", Data.Json.String "already_published");
      ]
  | Riot_publish.Planned prepared ->
      Data.Json.Object [
        ("status", Data.Json.String "planned");
        ("publish", prepared_publish_json ~workspace_root prepared);
      ]
  | Riot_publish.Published release ->
      Data.Json.Object [
        ("status", Data.Json.String "published");
        ("release", published_release_json release);
      ]

let write_json_line = fun json -> println (Data.Json.to_string json)

let write_publish_event_json = fun ~workspace_root event ->
  publish_event_to_json ~workspace_root event
  |> Option.for_each ~fn:write_json_line

let write_publish_error_json = fun error ->
  write_json_line
    (json_event
      "publish.error"
      [ ("message", Data.Json.String (Riot_publish.publish_error_message error)); ])

let write_publish_completed_json = fun ~workspace_root outcomes ->
  write_json_line
    (json_event
      "publish.completed"
      [
        ("outcomes", Data.Json.Array (List.map outcomes ~fn:(publish_outcome_json ~workspace_root)));
      ])

let run = fun (workspace: Workspace.t) matches ->
  let package_name =
    match ArgParser.get_one matches "package" with
    | None -> Ok None
    | Some package_name ->
        Package_name.from_string package_name
        |> Result.map ~fn:Option.some
        |> Result.map_err ~fn:(fun error -> Failure (Package_name.error_message error))
  in
  let* package_name = package_name in
  match resolve_request ~package_name ~workspace_mode:(ArgParser.get_flag matches "workspace") with
  | Error err -> fail err
  | Ok request ->
      let json = ArgParser.get_flag matches "json" in
      if json then
        Ui.reset_json_clock ~started_at:(Time.Instant.now ());
      let mode =
        if ArgParser.get_flag matches "dry-run" then
          Riot_publish.DryRun
        else
          Riot_publish.Publish
      in
      let ui = Ui.make ~mode:(Ui.mode_of_json_flag json) () in
      let displayed_packages = HashSet.create () in
      let progress =
        Ui.Common.{
          built_count = 0;
          cached_count = 0;
          failed_count = 0;
          skipped_count = 0;
        }
      in
      match Riot_publish.publish
        ~on_event:(
          if json then
            write_publish_event_json ~workspace_root:workspace.root
          else
            write_publish_event ~workspace_root:workspace.root ~ui ~displayed_packages ~progress
        )
        ~workspace
        ~request:(publish_request
          ~skip_fmt:(ArgParser.get_flag matches "skip-fmt")
          ~skip_check:(ArgParser.get_flag matches "skip-check")
          request)
        ~mode
        () with
      | Error err ->
          if json then
            write_publish_error_json err;
          fail (PublishFailed err)
      | Ok results ->
          if json then
            write_publish_completed_json ~workspace_root:workspace.root results;
          Ok ()
