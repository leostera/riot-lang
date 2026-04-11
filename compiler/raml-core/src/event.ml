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

type kind =
  | CompileStarted of { path: Path.t }
  | CompileFinished of { path: Path.t }
  | CompileFailed of { path: Path.t; message: string }
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

let backend_to_string = fun backend ->
  match backend with
  | CoreIr -> "core_ir"
  | Jir -> "jir"
  | Nir -> "nir"
  | Mir -> "mir"
  | Lir -> "lir"
  | Wasm -> "wasm"

let status_to_string = fun status ->
  match status with
  | Ok -> "ok"
  | Error -> "error"
  | Blocked -> "blocked"
  | Unavailable -> "unavailable"

let object_with_instant = fun instant_us fields ->
  Json.obj (fields @ [ ("instant_us", Json.int instant_us); ])

let to_json = fun event ->
  let instant_us = event.instant_us in
  match event.kind with
  | CompileStarted { path } -> object_with_instant
    instant_us
    [ ("type", Json.string "raml_compile_started"); ("path", Json.string (Path.to_string path)); ]
  | CompileFinished { path } -> object_with_instant
    instant_us
    [ ("type", Json.string "raml_compile_finished"); ("path", Json.string (Path.to_string path)); ]
  | CompileFailed { path; message } -> object_with_instant
    instant_us
    [
      ("type", Json.string "raml_compile_failed");
      ("path", Json.string (Path.to_string path));
      ("message", Json.string message);
    ]
  | SourceLoaded { path; unit_name; source_bytes } -> object_with_instant
    instant_us
    [
      ("type", Json.string "raml_source_loaded");
      ("path", Json.string (Path.to_string path));
      ("unit_name", Json.string unit_name);
      ("source_bytes", Json.int source_bytes);
    ]
  | TypingFinished {
    path;
    unit_name;
    completeness;
    parse_diagnostic_count;
    lowering_diagnostic_count;
    typing_diagnostic_count
  } -> object_with_instant
    instant_us
    [
      ("type", Json.string "raml_typing_finished");
      ("path", Json.string (Path.to_string path));
      ("unit_name", Json.string unit_name);
      ("completeness", Json.string completeness);
      ("parse_diagnostic_count", Json.int parse_diagnostic_count);
      ("lowering_diagnostic_count", Json.int lowering_diagnostic_count);
      ("typing_diagnostic_count", Json.int typing_diagnostic_count);
    ]
  | LoweringFinished { path; backend; status; error_count } -> object_with_instant
    instant_us
    [
      ("type", Json.string "raml_lowering_finished");
      ("path", Json.string (Path.to_string path));
      ("backend", Json.string (backend_to_string backend));
      ("status", Json.string (status_to_string status));
      ("error_count", Json.int error_count);
    ]
  | CodegenFinished { path; target; status } -> object_with_instant
    instant_us
    [
      ("type", Json.string "raml_codegen_finished");
      ("path", Json.string (Path.to_string path));
      ("target", Target.to_json target);
      ("status", Json.string (status_to_string status));
    ]
