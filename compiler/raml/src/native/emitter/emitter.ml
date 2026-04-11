open Std
open Std.Data

type error =
  | UnsupportedTarget of { target: Target.t }
  | UnsupportedProgram of { reason: string }

let error_to_json = fun error ->
  match error with
  | UnsupportedTarget { target } -> Json.obj
    [ ("kind", Json.string "unsupported_target"); ("target", Target.to_json target); ]
  | UnsupportedProgram { reason } -> Json.obj
    [ ("kind", Json.string "unsupported_program"); ("reason", Json.string reason); ]

let supports_aarch64_apple_darwin = fun target ->
  String.equal (Target.to_string target) "aarch64-apple-darwin"

let emit_program = fun ~host:_ ~target program ->
  if supports_aarch64_apple_darwin target then
    Result.map_error
      (fun reason -> UnsupportedProgram { reason })
      (Aarch64_apple_darwin.emit_program program)
  else
    Error (UnsupportedTarget { target })
