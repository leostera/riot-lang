open Std
open Std.Data
module Compiler_target = Raml_core.Target

type error =
  | UnsupportedTarget of { target: Compiler_target.t }
  | UnsupportedProgram of { reason: string }

let error_to_json = fun error ->
  match error with
  | UnsupportedTarget { target } -> Json.obj
    [ ("kind", Json.string "unsupported_target"); ("target", Compiler_target.to_json target); ]
  | UnsupportedProgram { reason } -> Json.obj
    [ ("kind", Json.string "unsupported_program"); ("reason", Json.string reason); ]

let supports_aarch64_apple_darwin = fun target ->
  String.equal (Compiler_target.to_string target) "aarch64-apple-darwin"

let emit_program = fun ~host:_ ~target program ->
  if supports_aarch64_apple_darwin target then
    Result.map_error
      (fun reason -> UnsupportedProgram { reason })
      (Aarch64_apple_darwin.emit_program program)
  else
    Error (UnsupportedTarget { target })
