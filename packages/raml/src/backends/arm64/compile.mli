open Std

val compile_lambda_to_asm :
  Lambda.Ir.lambda -> Path.t -> (unit, string) Result.t

val compile_lambda_to_executable :
  Lambda.Ir.lambda -> Path.t -> (unit, string) Result.t
