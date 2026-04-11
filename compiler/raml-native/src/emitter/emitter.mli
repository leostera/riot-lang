open Std

module Compiler_target = Raml_core.Target

type error =
  | UnsupportedTarget of { target: Compiler_target.t }
  | UnsupportedProgram of { reason: string }
val error_to_json: error -> Std.Data.Json.t

val emit_program:
  host:Compiler_target.t -> target:Compiler_target.t -> Lir.Program.t -> (string, error) result
