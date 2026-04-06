open Std

type t =
  | ExplainAndPath of { path: Path.t }
  | InvalidPath of { path: Path.t; reason: string }
  | NoTargets
  | TypecheckFailed
  | UnknownPackage of { package_name: string }
  | UnknownDiagnosticId of { diagnostic_id: string }
  | RegistryInitializationFailed of { registry: string; error: string }
  | WorkspacePreparationFailed of { path: Path.t; error: string }

let message = function
  | ExplainAndPath { path } -> "cannot use --explain together with a path ("
  ^ Path.to_string path
  ^ ")"
  | InvalidPath { path; reason } -> "invalid check path " ^ Path.to_string path ^ ": " ^ reason
  | NoTargets -> "no OCaml files found"
  | TypecheckFailed -> "typecheck failed"
  | UnknownPackage { package_name } -> "unknown workspace package: " ^ package_name
  | UnknownDiagnosticId { diagnostic_id } -> "unknown typ diagnostic id: " ^ diagnostic_id
  | RegistryInitializationFailed { registry; error } -> "failed to initialize registry '"
  ^ registry
  ^ "': "
  ^ error
  | WorkspacePreparationFailed { path; error } -> "failed to prepare workspace '"
  ^ Path.to_string path
  ^ "': "
  ^ error

let should_print = function
  | TypecheckFailed -> false
  | _ -> true
