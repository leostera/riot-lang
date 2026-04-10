open Std

module Target = Target

module Event: sig
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
  val to_json: t -> Std.Data.Json.t
end

module Config: sig
  type t = {
    on_event: (Event.t -> unit) option;
    host: Target.t;
    target: Target.t;
  }
  val default: t

  val make: ?on_event:(Event.t -> unit) -> ?host:Target.t -> ?target:Target.t -> unit -> t

  val with_on_event: t -> on_event:(Event.t -> unit) -> t

  val without_on_event: t -> t

  val with_host: t -> host:Target.t -> t

  val with_target: t -> target:Target.t -> t

  val with_targeting: t -> host:Target.t -> target:Target.t -> t

  val emit_event: t -> (unit -> Event.kind) -> unit
end

module Compilation: sig
  type t
  val to_json: t -> Std.Data.Json.t

  val lowering_to_json: t -> Std.Data.Json.t

  val codegen_to_json: t -> Std.Data.Json.t
end

module Core_ir = Core_ir

module Core_ir_fixture_support = Core_ir_fixture_support

module Example_pipeline = Example_pipeline

module Js = Js

module Native = Native

module Source_unit = Source_unit

module Typ_lowering = Typ_lowering

val compile_source: ?config:Config.t -> relpath:Std.Path.t -> string -> (Compilation.t, string) result

val compile: ?config:Config.t -> Std.Path.t -> (Compilation.t, string) result
