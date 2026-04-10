open Std

type t =
  | Start of { target_count: int }
  | WorkspacePrepared of { packages: (string * Path.t) list }
  | Package of { package_name: string }
  | PackageCached of { package_name: string }
  | PackagePlanningStarted of { package_name: string; include_dev: bool }
  | PackagePlanningFinished of {
      package_name: string;
      include_dev: bool;
      group_count: int;
      allowed_source_count: int
    }
  | PackageSourcePreparationStarted of {
      package_name: string;
      planning_root: Path.t;
      allowed_source_count: int;
      include_dev: bool
    }
  | PackageSourcePreparationFinished of {
      package_name: string;
      planning_root: Path.t;
      produced_source_count: int;
      generated_source_count: int
    }
  | PackageSourcePreparationFailed of { package_name: string; planning_root: Path.t; reason: string }
  | PackageSessionSeedStarted of {
      package_name: string;
      ordered_source_count: int;
      target_path_count: int
    }
  | PackageSessionSeedFinished of { package_name: string; prepared_source_count: int }
  | PackageRootGroupingFinished of {
      package_name: string;
      root_group_count: int;
      target_path_count: int
    }
  | PackageSnapshotPersistenceStarted of { package_name: string; root_target_count: int }
  | PackageSnapshotPersistenceFinished of { package_name: string; saved_module_count: int }
  | PackageSnapshotCheckedFilesStarted of { package_name: string; root_target_count: int }
  | PackageSnapshotCheckedFilesFinished of { package_name: string; checked_file_count: int }
  | PackageSnapshotReloadStarted of { package_name: string; root_target_count: int }
  | PackageSnapshotReloadFinished of {
      package_name: string;
      rooted_module_count: int;
      local_alias_typing_count: int;
      public_module_typing_count: int;
      loaded_module_count: int
    }
  | PackageCheckedGroupAssembleStarted of { package_name: string; target_path_count: int }
  | PackageCheckedGroupAssembleFinished of { package_name: string; checked_file_count: int }
  | PackageCheckedGroupEmitStarted of { package_name: string; checked_file_count: int }
  | PackageCheckedGroupEmitFinished of { package_name: string; checked_file_count: int }
  | Typ of { event: Typ.Event.t }
  | File of State.checked_file
  | Diagnostic of { path: Path.t; diagnostic_index: int; diagnostic: Diagnostic.t }
  | Summary of { summary: State.checked_summary }
  | Explanation of { explanation: Typ.Diagnostics.Explanations.t }

let checked_summary_to_json = fun (summary: State.checked_summary) ->
  Data.Json.Object [
    ("files", Data.Json.Int summary.checked_files);
    ("read_failures", Data.Json.Int summary.read_failures);
    ("diagnostics", Data.Json.Int summary.diagnostics);
    ("warnings", Data.Json.Int summary.warnings);
  ]

let read_report_to_json = fun ~workspace_root ~path reason ->
  Data.Json.Object [
    ("path", Data.Json.String (Scope.relative_or_absolute ~workspace_root path));
    ("ok", Data.Json.Bool false);
    ("error", Data.Json.String reason);
    ("diagnostics", Data.Json.Array []);
    (
      "summary",
      Data.Json.Object [
        ("parse", Data.Json.Int 0);
        ("lowering", Data.Json.Int 0);
        ("typing", Data.Json.Int 0);
        ("total", Data.Json.Int 0);
      ]
    );
  ]

let checked_file_to_json = fun ~workspace_root checked_file ->
  match checked_file with
  | State.Typed { path; report; diagnostics } ->
      let summary = Data.Json.Object [
        ("parse", Data.Json.Int (List.length report.parse_diagnostics));
        ("lowering", Data.Json.Int (List.length report.lowering_diagnostics));
        ("typing", Data.Json.Int (List.length report.typing_diagnostics));
        ("total", Data.Json.Int (List.length diagnostics));
      ] in
      Data.Json.Object [
        ("path", Data.Json.String (Scope.relative_or_absolute ~workspace_root path));
        ("ok", Data.Json.Bool (not (Diagnostic.has_errors diagnostics)));
        ("summary", summary);
      ]
  | State.Unreadable { path; reason } -> read_report_to_json ~workspace_root ~path reason

let diagnostic_events = function
  | State.Unreadable _ -> []
  | State.Typed { path; diagnostics; _ } -> diagnostics
  |> List.mapi (fun diagnostic_index diagnostic -> Diagnostic { path; diagnostic_index; diagnostic })

let to_json = fun ~workspace_root event ->
  match event with
  | Start { target_count } -> Data.Json.Object [
    ("type", Data.Json.String "check_start");
    ("workspace_root", Data.Json.String (Path.to_string workspace_root));
    ("target_count", Data.Json.Int target_count);
  ]
  | WorkspacePrepared { packages } -> Data.Json.Object [
    ("type", Data.Json.String "check_workspace_prepared");
    (
      "packages",
      Data.Json.Array (packages
      |> List.map
        (fun (package_name, package_root) ->
          Data.Json.Object [
            ("package_name", Data.Json.String package_name);
            (
              "package_root",
              Data.Json.String (Scope.relative_or_absolute ~workspace_root package_root)
            );
          ]))
    );
  ]
  | Package { package_name } -> Data.Json.Object [
    ("type", Data.Json.String "check_package");
    ("package_name", Data.Json.String package_name);
  ]
  | PackageCached { package_name } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_cached");
    ("package_name", Data.Json.String package_name);
  ]
  | PackagePlanningStarted { package_name; include_dev } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_planning_start");
    ("package_name", Data.Json.String package_name);
    ("include_dev", Data.Json.Bool include_dev);
  ]
  | PackagePlanningFinished { package_name; include_dev; group_count; allowed_source_count } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_planning_finish");
    ("package_name", Data.Json.String package_name);
    ("include_dev", Data.Json.Bool include_dev);
    ("group_count", Data.Json.Int group_count);
    ("allowed_source_count", Data.Json.Int allowed_source_count);
  ]
  | PackageSourcePreparationStarted {
    package_name;
    planning_root;
    allowed_source_count;
    include_dev
  } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_source_preparation_start");
    ("package_name", Data.Json.String package_name);
    ("planning_root", Data.Json.String (Path.to_string planning_root));
    ("allowed_source_count", Data.Json.Int allowed_source_count);
    ("include_dev", Data.Json.Bool include_dev);
  ]
  | PackageSourcePreparationFinished {
    package_name;
    planning_root;
    produced_source_count;
    generated_source_count
  } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_source_preparation_finish");
    ("package_name", Data.Json.String package_name);
    ("planning_root", Data.Json.String (Path.to_string planning_root));
    ("produced_source_count", Data.Json.Int produced_source_count);
    ("generated_source_count", Data.Json.Int generated_source_count);
  ]
  | PackageSourcePreparationFailed { package_name; planning_root; reason } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_source_preparation_failed");
    ("package_name", Data.Json.String package_name);
    ("planning_root", Data.Json.String (Path.to_string planning_root));
    ("reason", Data.Json.String reason);
  ]
  | PackageSessionSeedStarted { package_name; ordered_source_count; target_path_count } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_session_seed_start");
    ("package_name", Data.Json.String package_name);
    ("ordered_source_count", Data.Json.Int ordered_source_count);
    ("target_path_count", Data.Json.Int target_path_count);
  ]
  | PackageSessionSeedFinished { package_name; prepared_source_count } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_session_seed_finish");
    ("package_name", Data.Json.String package_name);
    ("prepared_source_count", Data.Json.Int prepared_source_count);
  ]
  | PackageRootGroupingFinished { package_name; root_group_count; target_path_count } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_root_grouping_finish");
    ("package_name", Data.Json.String package_name);
    ("root_group_count", Data.Json.Int root_group_count);
    ("target_path_count", Data.Json.Int target_path_count);
  ]
  | PackageSnapshotPersistenceStarted { package_name; root_target_count } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_snapshot_persistence_start");
    ("package_name", Data.Json.String package_name);
    ("root_target_count", Data.Json.Int root_target_count);
  ]
  | PackageSnapshotPersistenceFinished { package_name; saved_module_count } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_snapshot_persistence_finish");
    ("package_name", Data.Json.String package_name);
    ("saved_module_count", Data.Json.Int saved_module_count);
  ]
  | PackageSnapshotCheckedFilesStarted { package_name; root_target_count } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_snapshot_checked_files_start");
    ("package_name", Data.Json.String package_name);
    ("root_target_count", Data.Json.Int root_target_count);
  ]
  | PackageSnapshotCheckedFilesFinished { package_name; checked_file_count } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_snapshot_checked_files_finish");
    ("package_name", Data.Json.String package_name);
    ("checked_file_count", Data.Json.Int checked_file_count);
  ]
  | PackageSnapshotReloadStarted { package_name; root_target_count } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_snapshot_reload_start");
    ("package_name", Data.Json.String package_name);
    ("root_target_count", Data.Json.Int root_target_count);
  ]
  | PackageSnapshotReloadFinished {
    package_name;
    rooted_module_count;
    local_alias_typing_count;
    public_module_typing_count;
    loaded_module_count
  } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_snapshot_reload_finish");
    ("package_name", Data.Json.String package_name);
    ("rooted_module_count", Data.Json.Int rooted_module_count);
    ("local_alias_typing_count", Data.Json.Int local_alias_typing_count);
    ("public_module_typing_count", Data.Json.Int public_module_typing_count);
    ("loaded_module_count", Data.Json.Int loaded_module_count);
  ]
  | PackageCheckedGroupAssembleStarted { package_name; target_path_count } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_checked_group_assemble_start");
    ("package_name", Data.Json.String package_name);
    ("target_path_count", Data.Json.Int target_path_count);
  ]
  | PackageCheckedGroupAssembleFinished { package_name; checked_file_count } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_checked_group_assemble_finish");
    ("package_name", Data.Json.String package_name);
    ("checked_file_count", Data.Json.Int checked_file_count);
  ]
  | PackageCheckedGroupEmitStarted { package_name; checked_file_count } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_checked_group_emit_start");
    ("package_name", Data.Json.String package_name);
    ("checked_file_count", Data.Json.Int checked_file_count);
  ]
  | PackageCheckedGroupEmitFinished { package_name; checked_file_count } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_checked_group_emit_finish");
    ("package_name", Data.Json.String package_name);
    ("checked_file_count", Data.Json.Int checked_file_count);
  ]
  | Typ { event } -> Typ.Event.to_json event
  | File checked_file -> Data.Json.Object [
    ("type", Data.Json.String "check_file");
    ("result", checked_file_to_json ~workspace_root checked_file);
  ]
  | Diagnostic { path; diagnostic_index; diagnostic } -> Data.Json.Object [
    ("type", Data.Json.String "check_diagnostic");
    ("path", Data.Json.String (Scope.relative_or_absolute ~workspace_root path));
    ("diagnostic_index", Data.Json.Int diagnostic_index);
    ("diagnostic", Diagnostic.to_json diagnostic);
  ]
  | Summary { summary } -> Data.Json.Object [
    ("type", Data.Json.String "check_summary");
    ("ok", Data.Json.Bool (not summary.has_error));
    ("summary", checked_summary_to_json summary);
  ]
  | Explanation { explanation } -> Typ.Diagnostics.Explanations.to_json explanation
