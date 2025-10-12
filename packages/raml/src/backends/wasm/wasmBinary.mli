val encode_module : WasmIR.wasm_module -> int list
val write_binary : Std.Path.t -> WasmIR.wasm_module -> (unit, Std.Fs.error) result
