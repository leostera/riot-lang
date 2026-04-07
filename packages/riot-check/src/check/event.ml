open Std

type t =
  | Start of { target_count: int }
  | Package of { package_name: string }
  | PackageCached of { package_name: string }
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
  | Package { package_name } -> Data.Json.Object [
    ("type", Data.Json.String "check_package");
    ("package_name", Data.Json.String package_name);
  ]
  | PackageCached { package_name } -> Data.Json.Object [
    ("type", Data.Json.String "check_package_cached");
    ("package_name", Data.Json.String package_name);
  ]
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
