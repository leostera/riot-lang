open Std
open Std.Data

type backend =
  | CoreIr
  | Jir
  | Nir
  | Mir
  | Lir
  | Wasm
type status =
  | Ok
  | Error
  | Blocked
  | Unavailable
type failure =
  | ConfigValidationFailed of { reason: string }
  | SourceReadFailed of { reason: string }
  | SourceUnitRejected of { reason: string }
type kind =
  | CompileStarted of { path: Path.t }
  | CompileFinished of { path: Path.t }
  | CompileFailed of { path: Path.t; failure: failure }
  | SourceLoaded of { path: Path.t; unit_name: string; source_bytes: int }
  | TypingFinished of {
      path: Path.t;
      unit_name: string;
      completeness: string;
      parse_diagnostic_count: int;
      lowering_diagnostic_count: int;
      typing_diagnostic_count: int
    }
  | LoweringFinished of { path: Path.t; backend: backend; status: status; error_count: int }
  | CodegenFinished of { path: Path.t; target: Target.t; status: status }
type t = {
  instant_us: int;
  kind: kind;
}
val to_string: t -> string

val to_json: t -> Json.t
