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

      let assemble_result =
        Command.make "as" ~args:[ "-o"; output_str ^ ".o"; asm_str ]
        |> Command.output
      in

      match assemble_result with
      | Error _err -> Error "Assembly failed"
      | Ok _ -> (
          let link_result =
            Command.make "ld"
              ~args:
                [
                  "-o";
                  output_str;
                  output_str ^ ".o";
                  "-lSystem";
                  "-syslibroot";
                  "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk";
                  "-e";
                  "_main";
                  "-arch";
                  "arm64";
                ]
            |> Command.output
          in

          match link_result with
          | Error _err -> Error "Linking failed"
          | Ok _ -> Ok ()))
