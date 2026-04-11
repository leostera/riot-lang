open Std

module Backend_result = RamlCore.Backend_result
module Frontend_pipeline = RamlCore.Frontend_pipeline

let compile = fun ~config:_ ~(frontend: Frontend_pipeline.t) ->
  let core_ir = Frontend_pipeline.core_ir frontend in
  match core_ir.value with
  | None -> Backend_result.blocked_wasm ~blocked_on:"core_ir" core_ir.errors
  | Some _ -> Backend_result.unavailable_wasm ~message:"wasm backend not implemented"
