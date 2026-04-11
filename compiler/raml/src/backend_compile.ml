module type S = sig
  val compile: config:Config.t -> frontend:Frontend_pipeline.t -> Backend_result.t
end

module Wasm_backend = struct
  let compile = fun ~config:_ ~(frontend: Frontend_pipeline.t) ->
    let core_ir = Frontend_pipeline.core_ir frontend in
    match core_ir.value with
    | None -> Backend_result.blocked_wasm ~blocked_on:"core_ir" core_ir.errors
    | Some _ -> Backend_result.unavailable_wasm ~message:"wasm backend not implemented"
end

let select = fun ~config ->
  match Config.select_backend config with
  | Target.Js -> (module Js_backend : S)
  | Target.Native -> (module Native_backend : S)
  | Target.Wasm -> (module Wasm_backend : S)
