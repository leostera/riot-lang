open Std
module Target = RamlCore.Target

type error =
  | UnsupportedTarget of { target: Target.t }
  | UnsupportedProgram of { reason: string }
val error_to_json: error -> Std.Data.Json.t

val emit_program: host:Target.t -> target:Target.t -> Lir.Program.t -> (string, error) result
