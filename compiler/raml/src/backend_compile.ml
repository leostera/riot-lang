module Config = Raml_core.Config
module Frontend_pipeline = Raml_core.Frontend_pipeline
module Backend_result = Raml_core.Backend_result
module Target = Raml_core.Target

module type S = sig
  val compile: config:Config.t -> frontend:Frontend_pipeline.t -> Backend_result.t
end

let select = fun ~config ->
  match Config.select_backend config with
  | Target.Js -> (module Js_backend : S)
  | Target.Native -> (module Raml_native.Backend : S)
  | Target.Wasm -> (module Raml_wasm.Backend : S)
