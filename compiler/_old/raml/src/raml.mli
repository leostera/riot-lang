open Std

module Target = Raml_core.Target

module Event = Raml_core.Event

module Config = Raml_core.Config

module Compilation = Raml_core.Compilation

module CoreIR = Raml_core.Core_ir

module Js = Raml_js.Js

module Native = Raml_native.Native

module Source_unit = Raml_core.Source_unit

module Typ_lowering = Raml_core.Typ_lowering

val compile: ?config:Config.t -> Std.Path.t -> (Compilation.t, string) Std.Result.t

module TestingHelpers: sig
  val compile_source:
    ?config:Config.t -> relpath:Std.Path.t -> string -> (Compilation.t, string) Std.Result.t

  module Test_fixture_typing: sig
    val raml_config: host:Target.t -> target:Target.t -> Config.t
  end

  module Example_pipeline: sig
    type t
    val compile_source:
      config:Config.t -> relpath:Std.Path.t -> source:string -> (t, string) Std.Result.t

    val to_json: t -> Std.Data.Json.t

    val lowering_to_json: t -> Std.Data.Json.t

    val codegen_to_json: t -> Std.Data.Json.t
  end

  module Core_ir_fixture_support: sig
    val parse_compilation_unit: Std.Data.Json.t -> (CoreIR.Compilation_unit.t, string) Std.Result.t
  end
end
