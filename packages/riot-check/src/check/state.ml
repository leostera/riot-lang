open Std

module Typ_check_result = Typ.Analysis.Check_result

type checked_file =
  | Typed of {
      path: Path.t;
      report: Typ_check_result.t;
      diagnostics: Diagnostic.t list;
    }
  | Unreadable of {
      path: Path.t;
      reason: string;
    }

type checked_summary = {
  checked_files: int;
  read_failures: int;
  diagnostics: int;
  warnings: int;
  has_error: bool;
}

type check_run_summary = { target_count: int; summary: checked_summary }

let empty_checked_summary = {
  checked_files = 0;
  read_failures = 0;
  diagnostics = 0;
  warnings = 0;
  has_error = false;
}

let checked_file_path = function
  | Typed { path; _ }
  | Unreadable { path; _ } -> path

let update_checked_summary = fun summary checked_file ->
  match checked_file with
  | Unreadable _ ->
      {
        checked_files = summary.checked_files + 1;
        read_failures = summary.read_failures + 1;
        diagnostics = summary.diagnostics;
        warnings = summary.warnings;
        has_error = true;
      }
  | Typed { diagnostics; _ } ->
      let warning_count =
        diagnostics
        |> List.filter Diagnostic.has_warning_diagnostic
        |> List.length
      in
      {
        checked_files = summary.checked_files + 1;
        read_failures = summary.read_failures;
        diagnostics = summary.diagnostics + List.length diagnostics;
        warnings = summary.warnings + warning_count;
        has_error = summary.has_error || Diagnostic.has_errors diagnostics;
      }
