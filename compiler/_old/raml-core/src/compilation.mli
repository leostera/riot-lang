open Std.Data

type frontend_diagnostic =
  | Parse of Syn.Diagnostic.t
  | Lowering of Typ.Diagnostics.Diagnostic.t
  | Typing of Typ.Diagnostics.Diagnostic.t
type t
val create:
  targeting:Json.t ->
  source:Json.t ->
  typing:Json.t ->
  core_ir:Json.t ->
  frontend_diagnostics:frontend_diagnostic list ->
  lowering_fields:(string * Json.t) list ->
  codegen_fields:(string * Json.t) list ->
  t

val from_pipeline_json: Json.t -> t

val to_json: t -> Json.t

val lowering_to_json: t -> Json.t

val codegen_to_json: t -> Json.t

val frontend_diagnostics: t -> frontend_diagnostic list

val has_frontend_errors: t -> bool

val emitted_output: t -> (string, string) Std.Result.t
