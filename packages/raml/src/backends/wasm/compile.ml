open Std

let compile_lambda_to_wasm expr output_path =
  let wasm_ir = WasmIR.translate_from_lambda expr in

  let wat_path = Path.v (Path.to_string output_path ^ ".wat") in
  let wat_content = WasmAST.to_wat wasm_ir in
  match Fs.write wat_content wat_path with
  | Error err ->
      Error (format "Failed to write WAT file: %s" (IO.error_message err))
  | Ok () -> (
      match WasmBinary.write_binary output_path wasm_ir with
      | Ok () -> Ok ()
      | Error err ->
          Error (format "Failed to write WASM binary: %s" (IO.error_message err)))
