open Std

type error =
  | Unsupported_target of { target: Target.t }
  | Unsupported_program of { reason: string }
val error_to_json: error -> Std.Data.Json.t

val emit_program: host:Target.t -> target:Target.t -> Lir.Program.t -> (string, error) result
