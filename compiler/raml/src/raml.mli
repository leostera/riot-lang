open Std

module Target = Raml_core.Target

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
    typing_config: Typ.Config.t;
  }
  val default: t

  val validate: t -> (unit, string) Std.Result.t

  val make:
    ?on_event:(Event.t -> unit) ->
    ?host:Target.t ->
    ?target:Target.t ->
    ?typing_config:Typ.Config.t ->
    unit ->
    t

  val with_on_event: t -> on_event:(Event.t -> unit) -> t

  val without_on_event: t -> t

  val with_host: t -> host:Target.t -> t

  val with_target: t -> target:Target.t -> t

  val with_targeting: t -> host:Target.t -> target:Target.t -> t

  val with_typing_config: t -> typing_config:Typ.Config.t -> t

  val host: t -> Target.t

  val target: t -> Target.t

  val typing_config: t -> Typ.Config.t

  val select_backend: t -> Target.backend

  val emit_event: t -> (unit -> Event.kind) -> unit
end

module Compilation = Raml_core.Compilation

module CoreIR = Raml_core.Core_ir

module Js = Raml_js.Js

module Native = Raml_native.Native

module Source_unit = Raml_core.Source_unit

module Typ_lowering = Raml_core.Typ_lowering

val compile: ?config:Config.t -> Std.Path.t -> (Compilation.t, string) Std.Result.t

module TestingHelpers: sig
  val compile_source:
    ?config:Config.t ->
    relpath:Std.Path.t ->
    string ->
    (Compilation.t, string) Std.Result.t

  module Test_fixture_typing: sig
    val typing_config: Typ.Config.t

    val raml_config:
      host:Target.t ->
      target:Target.t ->
      Config.t
  end

  module Example_pipeline: sig
    type t
    val compile_source:
      config:Config.t ->
      relpath:Std.Path.t ->
      source:string ->
      (t, string) Std.Result.t

    val to_json: t -> Std.Data.Json.t

    val lowering_to_json: t -> Std.Data.Json.t

    val codegen_to_json: t -> Std.Data.Json.t
  end

  module Core_ir_fixture_support: sig
    val parse_compilation_unit:
      Std.Data.Json.t ->
      (CoreIR.Compilation_unit.t, string) Std.Result.t
  end
end
