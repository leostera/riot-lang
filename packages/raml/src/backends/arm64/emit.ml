open Std

let emit_to_string instrs =
  let lines = List.map Instruction.instruction_to_string instrs in
  String.concat "\n" lines ^ "\n"

let emit_to_file instrs output_path =
  let asm_code = emit_to_string instrs in
  Fs.write asm_code output_path
