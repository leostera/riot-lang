open Std

val emit_to_file :
  Instruction.instruction list -> Path.t -> (unit, Fs.error) Result.t

val emit_to_string : Instruction.instruction list -> string
