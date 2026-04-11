module Config = RamlCore.Config
module Frontend_pipeline = RamlCore.Frontend_pipeline
module Backend_result = RamlCore.Backend_result
module Target = RamlCore.Target

module type S = sig
  val compile: config:Config.t -> frontend:Frontend_pipeline.t -> Backend_result.t
end

let select = fun ~config ->
  match Config.select_backend config with
  | Target.Js -> (module Js_backend : S)
  | Target.Native -> (module RamlNative.Backend : S)
  | Target.Wasm -> (module RamlWasm.Backend : S)
