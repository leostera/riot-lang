open Std

module Compiler_target = Raml_core.Target

type error =
  | UnsupportedTargetArchitecture of {
      target: Compiler_target.t;
      supported_targets: Compiler_target.t list
    }
  | Aarch64_apple_darwin of Aarch64_apple_darwin.error
val error_to_json: error -> Std.Data.Json.t

val emit_program:
  host:Compiler_target.t -> target:Compiler_target.t -> Lir.Program.t -> (string, error) result
