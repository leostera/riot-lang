open Std
open Std.Data

type error =
  | Unsupported_target of { target: Target.t }
  | Unsupported_program of { reason: string }

let error_to_json = fun error ->
  match error with
  | Unsupported_target { target } -> Json.obj
    [ ("kind", Json.string "unsupported_target"); ("target", Target.to_json target); ]
  | Unsupported_program { reason } -> Json.obj
    [ ("kind", Json.string "unsupported_program"); ("reason", Json.string reason); ]

let supports_aarch64_apple_darwin = fun target ->
  String.equal (Target.to_string target) "aarch64-apple-darwin"

let emit_program = fun ~host:_ ~target program ->
  if supports_aarch64_apple_darwin target then
    Result.map_error
      (fun reason -> Unsupported_program { reason })
      (Aarch64_apple_darwin.emit_program program)
  else
    Error (Unsupported_target { target })
