open Std

let compile_lambda_to_asm lambda output_path =
  let instrs = Codegen.compile_program lambda in
  Emit.emit_to_file instrs output_path
  |> Result.map_err (fun _err -> "Failed to write assembly file")

let compile_lambda_to_executable lambda output_path =
  let asm_path = Path.v (Path.to_string output_path ^ ".s") in

  match compile_lambda_to_asm lambda asm_path with
  | Error msg -> Error msg
  | Ok () -> (
      let output_str = Path.to_string output_path in
      let asm_str = Path.to_string asm_path in

      let compile_result =
        Command.make "clang"
          ~args:[ "-target"; "x86_64-apple-darwin"; "-o"; output_str; asm_str ]
        |> Command.output
      in

      match compile_result with
      | Error _err -> Error "Compilation failed (is clang installed?)"
      | Ok _ -> Ok ())
